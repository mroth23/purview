package org.purview.copymoveanalyser

import java.awt.geom.Area
import java.awt.geom.Rectangle2D
import org.purview.core.analysis.Analyser
import org.purview.core.analysis.Settings
import org.purview.core.analysis.Metadata
import org.purview.core.analysis.settings.IntRangeSetting
import org.purview.core.analysis.settings.BooleanSetting
import org.purview.core.data.Color
import org.purview.core.data.ImageMatrix
import org.purview.core.data.ImmutableMatrix
import org.purview.core.data.Matrix
import org.purview.core.data.MutableArrayMatrix
import org.purview.core.report.Error
import org.purview.core.report.ReportEntry
import org.purview.core.report.ReportShapeMove
import org.purview.core.transforms.Fragmentize
import org.purview.core.transforms.ShapeToReportShape
import org.purview.core.transforms.Interpolate
import scala.collection.mutable.ArrayBuffer
import scala.math._
import scala.util.Sorting

class AnalyserImplementation extends Analyser[ImageMatrix] with Settings with Metadata {
  val name = "Copy-move analyser"
  val description = "Finds cloned regions in an image"
  override val version = Some("1.1")
  override val author = Some("Moritz Roth & David Flemström")

  override val iconResource = Some("icons/analysers/copy-move.png")

  val qualitySetting = IntRangeSetting("Quality", 0, 100)
  qualitySetting.value = 75 //default
  val thresholdSetting = IntRangeSetting("Threshold", 1, 1000)
  thresholdSetting.value = 30
  val minDistanceSetting = IntRangeSetting("Min. distance", 1, 100)
  minDistanceSetting.value = 20
  val partialDCTBlockSizeSetting = IntRangeSetting("Partial DCT block size", 2, 16)
  partialDCTBlockSizeSetting.value = 3
  val blockSizeSetting = IntRangeSetting("Block size", 2, 16)
  blockSizeSetting.value = 16
  val autoQualitySetting = BooleanSetting("Automatically detect JPEG quality")
  autoQualitySetting.value = false

  val settings = List(qualitySetting, autoQualitySetting, blockSizeSetting, thresholdSetting,
                      minDistanceSetting, partialDCTBlockSizeSetting)

  /**
   * Represents the equivalent JPEG-quality that is to be used for the quantization of the DCT coefficients
   */
  private def quality = qualitySetting.value

  /**
   * The minimum number of regions that have to be moved in the same direction
   * for it to count as a shift
   */
  private def threshold = thresholdSetting.value

  /**
   * The minimum distance something has to be moved for it to count as a shift
   */
  private def minDistance = minDistanceSetting.value

  /**
   * How much of the DCT should we calculate?
   */
  private def partialDCTBlockSize = partialDCTBlockSizeSetting.value

  /**
   * The block size
   */
  private def blockSize = blockSizeSetting.value

  /**
   * Detect JPEG quality automatically?
   */
  private def autoDetectQuality = autoQualitySetting.value

  val quantTables = input.map{in =>
      in.metadata.get("DQT").map { dqt =>
        //println("Got DQT: " + dqt)
        dqt.keySet.toSeq.sortBy(identity).map(key => dqt(key).split(',').map(_.toInt).toSeq)
      }.getOrElse(Nil)
    }

  /** Stretches the quantization matrix according to a quality value */
  @inline private def createQTable(q: Int, size: Int): Matrix[Float] = {
    status("Creating quantization tables with quality " + q.toString())
    println("Creating quantization tables with quality " + q.toString())
    //These magic numbers come from the standard JPEG algorithm
    val s = if(q < 50) 5000f / q else 200f - 2f * q
    //The "standard" luminance quantization table taken from the JPEG specification (Annex K1)
    //We change the values according to the quality factor
    var coefficients = Array(
      16f,  11f,  10f,  16f,  24f,  40f,  51f,  61f,
      12f,  12f,  14f,  19f,  26f,  58f,  60f,  55f,
      14f,  13f,  16f,  24f,  40f,  57f,  69f,  56f,
      14f,  17f,  22f,  29f,  51f,  87f,  80f,  62f,
      18f,  22f,  37f,  56f,  68f, 109f, 103f,  77f,
      24f,  35f,  55f,  64f,  81f, 104f, 113f,  92f,
      49f,  64f,  78f,  87f, 103f, 121f, 120f, 101f,
      72f,  92f,  95f,  98f, 112f, 100f, 103f,  99f) map (x => (s * x + 50f) / 100f)
    //The DC coefficient will change with the DCT block size
    //Thus we need to change the DC quantization value accordingly
    coefficients(0) *= (size / 8)
    //Make the 8x8 quantization table
    val quant8 = ImmutableMatrix[Float](8, 8, coefficients)
    //Interpolate it to the "real" block size
    val interpolator = Interpolate(size, size)
    interpolator(quant8)
  }

  def estimateQuality(qt: Seq[Seq[Int]]) : Int = {
    val avY = (qt(0).sum - qt(0)(0)) / (qt(0).length - 1)
    val avC = (qt(1).sum - qt(1)(0)) / (qt(1).length - 1)

    val compression = (avY + 2 * avC) / 3
    val conversion = (avY - avC).abs * 0.49 * 2

    (90 - compression + conversion).abs.toInt
  }

  /** Converts the given color matrix into a float matrix of grayscale values */
  def grayscale(in: Matrix[Color]): Matrix[Float] = {
    status("Converting image to high-amplitude grayscale")
    in map (color => (color.redByte * 11 + color.greenByte * 16 + color.blueByte * 5) / 32 - 128f)
  }

  /** Quantizes the given matrix with the given quantization matrix */
  def quantize(input: Matrix[Float], quant: Matrix[Float]): Matrix[Float] = {
    val w = input.width
    val h = input.height
    val result = new MutableArrayMatrix[Float](w, h)
    var y = 0
    while(y < h) {
      var x = 0
      while(x < w) {
        result(x, y) = round(input(x, y) / quant(x, y))
        x += 1
      }
      y += 1
    }
    result
  }

  /** Partially calculates "the DCT matrix" for an input matrix */
  def partialDCT(input: Matrix[Float], size: Int, bs: Int, coefficients: Matrix[Float]): Matrix[Float] = {
    val w = input.width
    val h = input.height
    val result = new MutableArrayMatrix[Float](size, size)
    val pi = 3.141592653589793f
    status("Computing partial DCT blocks")

    var v = 0
    while(v < size) {
      var u = 0
      while(u < size) {

        var sum = 0f
        //Actually calculate the local sum
        var y = 0
        while(y < h) {
          var x = 0
          while(x < w) {
            sum += input(x, y) * (cos(pi * u * (2.0 * x + 1.0) / (bs * 2)) * cos(pi * v * (2.0 * y + 1.0) / (bs * 2))).toFloat
            x += 1
          }
          y += 1
        }
        sum *= coefficients(u, v)
        result(u, v) = sum
        u += 1
      }
      v += 1
    }
    result
  }

  /** Create quantized blocks for each cell in the input matrix */
  val makeBlocks = for(qt <- quantTables; in <- fragments) yield {
    status("Splitting up the image into DCT blocks with size " + partialDCTBlockSize)
    var quant = (if (autoDetectQuality == true && qt.length != 0){
        createQTable(estimateQuality(qt), blockSize)
      } else {
        createQTable(quality, blockSize)
      })
    val coefficients = new Matrix[Float] {
      val width = blockSize
      val height = blockSize

      private val Double = 1f / blockSize.toFloat //sqrt(1 / 16) * sqrt(1 / 16)
      private val Sans = 2f / blockSize.toFloat //sqrt(2 / 16f) * sqrt(2 / 16f)
      private val Single = sqrt(Double).toFloat * sqrt(Sans).toFloat  //sqrt(1 / 16f) * sqrt(2 / 16f)
    
      //Don't actually store the matrix; select values on the fly
      def apply(x: Int, y: Int) = if(x == 0 && y == 0)
        Double
      else if(x == 1 || y == 1)
        Single
      else
        Sans
    }

    val res: Iterable[Block] = for((x, y, value) <- in.cells) yield {
      new Block(quantize(partialDCT(value, partialDCTBlockSize, blockSize, coefficients), quant), x, y, partialDCTBlockSize)
    }
    res.toArray
  }

  def sortBlocks(blocks: Array[Block]) = {
    status("Sorting the generated blocks by similarity")
    Sorting.quickSort(blocks)
    blocks
  }

  def makeShifts(blocks: Array[Block]): Seq[Shift] = {
    status("Calculating block shifts")
    val result = new ArrayBuffer[Shift](blocks.length / blockSize)
    val minimumDistanceSquared = minDistance * minDistance
    //Go through each pair of blocks
    var i = 0
    while(i < blocks.length - 1) {
      val block1 = blocks(i)
      val block2 = blocks(i + 1)
      progress(i.toFloat / blocks.length)

      //If the blocks are identical (or near enough...), create a shift
      if((block1 compare block2) == 0) {
        //Use the upper-left-most block as the first block
        val shift = if(block1.y < block2.y)
          new Shift(block1, block2)
        else if(block1.y == block2.y && block1.x < block2.x)
          new Shift(block1, block2)
        else
          new Shift(block2, block1)

        //Only add it if the shift is long enough
        if(shift.lengthSquared > minimumDistanceSquared)
          result += shift //TODO: optimize! Why create the shift if we can calculate the length beforehand?
      }
      i += 1
    }
    result
  }

  def groupShifts(shifts: Seq[Shift]): Map[(Int, Int), Seq[Shift]] = {
    status("Grouping shifts by displacement vector")
    shifts.groupBy(_.vector).toMap
  }

  def makeReport(groupedShifts: Map[(Int, Int), Seq[Shift]]): Set[ReportEntry] = {
    status("Merging resulting vector regions")
    (for {
        (vector, shifts) <- groupedShifts
        if shifts.length > threshold //Only select shifts that have a large magnitude
      } yield {
        //Find the average x, y coordinates among blocks
        val avgX = shifts.map(_.from.x).foldLeft(0f)((acc, n) => acc + n.toFloat / shifts.length)
        val avgY = shifts.map(_.from.y).foldLeft(0f)((acc, n) => acc + n.toFloat / shifts.length)

        //Convert blocks into two sets of areas: "from" areas and "to" areas
        val rectsFrom: Seq[Area] =
          for(s <- shifts) yield new Area(new Rectangle2D.Double(s.from.x, s.from.y, blockSize, blockSize))

        val rectsTo: Seq[Area] =
          for(s <- shifts) yield new Area(new Rectangle2D.Double(s.to.x, s.to.y, blockSize, blockSize))

        //Merge the areas
        val areaFrom = new Area
        rectsFrom.foreach(areaFrom.add)

        val areaTo = new Area
        rectsTo.foreach(areaTo.add)

        new ReportShapeMove(Error, "This region was moved", ShapeToReportShape()(areaFrom), ShapeToReportShape()(areaTo))
      }
    ).toSet
  }

  /*
   * Process:
   * - Convert image to grayscale
   * - Fragmentize the image into overlapping 16x16 pieces
   * - Make blocks out of these pieces (also quantizing them while we're at it)
   * - Sort the blocks with quickSort
   * - Find blocks that are similar and make BlockShifts out of them
   * - Filter out block shifts that are improbable results
   * - Report the remaining shifts
   */

  val fragments = input >- grayscale >- Fragmentize(blockSize, blockSize)

  val result = makeBlocks >- sortBlocks >- makeShifts >- groupShifts >- makeReport
}

/**
 * Represents a block of some sort at the specified location
 */
sealed case class Block(values: Matrix[Float], x: Int, y: Int, size: Int) extends Ordered[Block] {
  require(values.width == size && values.height == size,
          "Block with invalid value matrix created! (Must be a square matrix of the correct size)")

  def compare(that: Block): Int = {
    val n = if(this.size < that.size) this.size else that.size

    var y = 0
    while(y < n) {
      var x = 0
      while(x < n) {
        val a = this.values(x, y)
        val b = that.values(x, y)

        //Please forgive me for using "return" in a functional language...
        if(a < b)
          return -1
        else if(a > b)
          return 1
        x += 1
      }
      y += 1
    }
    0
  }

  override def equals(x: Any): Boolean = x match {
    case that: Block =>
      var x = 0
      while(x < size) {
        var y = 0
        while(y < size) {
          if(this.values(x, y) != that.values(x, y))
            return false
          y += 1
        }
        x += 1
      }
      true
    case _ => false
  }

  //Simple symmetric hash code (all cell values are XOR'ed)
  override def hashCode = values.foldLeft(0)(_ ^ _.hashCode)
}

/**
 * Represents a shift from one block to another block
 */
sealed case class Shift(from: Block, to: Block) {
  val lengthSquared = (from.x - to.x) * (from.x - to.x) + (from.y - to.y) * (from.y - to.y)
  val vector = {
    val x1 = from.x - to.x
    val y1 = from.y - to.y
    if(y1 < 0) (-x1, -y1) else (x1, y1)
  }
}
