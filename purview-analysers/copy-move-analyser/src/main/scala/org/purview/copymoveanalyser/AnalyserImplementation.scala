package org.purview.copymoveanalyser

import java.awt.geom.Area
import java.awt.geom.Rectangle2D
import org.purview.core.analysis.Analyser
import org.purview.core.analysis.Settings
import org.purview.core.analysis.settings.IntRangeSetting
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
import scala.collection.mutable.ArrayBuffer
import scala.math._
import scala.util.Sorting

class AnalyserImplementation extends Analyser[ImageMatrix] with Settings {
  val name = "Copy-move analyser"
  val description = "Finds cloned regions in an image"
  override val version = Some("1.0")
  override val author = Some("Moritz Roth & David Flemström")

  override val iconResource = Some("icons/analysers/copy-move.png")

  val qualitySetting = IntRangeSetting("Quality", 0, 100)
  qualitySetting.value = 80 //default
  val thresholdSetting = IntRangeSetting("Threshold", 1, 1000)
  thresholdSetting.value = 40
  val minDistanceSetting = IntRangeSetting("Min. distance", 1, 100)
  minDistanceSetting.value = 25
  val partialDCTBlockSizeSetting = IntRangeSetting("Partial DCT block size", 2, 16)
  partialDCTBlockSizeSetting.value = 3

  val settings = List(qualitySetting, thresholdSetting,
                      minDistanceSetting, partialDCTBlockSizeSetting)

  /**
   * Represents the equivalent JPEG-quality that is to be used for the DCT transforms
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
   * Our quantization matrix (unstretched)
   * This matrix was expanded by replicating edge values of the "standard" JPEG matrix
   */
  private val quant16 =
    ImmutableMatrix[Float](16, 16, Array(
        032.0f,  30.0f,  35.0f,  35.0f,  45.0f,  60.0f, 122.5f, 180.0f, 180.0f, 180.0f, 180.0f, 180.0f, 180.0f, 180.0f, 180.0f, 180.0f,
        027.5f,  30.0f,  32.5f,  42.5f,  55.0f,  87.5f, 160.0f, 230.0f, 230.0f, 230.0f, 230.0f, 230.0f, 230.0f, 230.0f, 230.0f, 230.0f,
        025.0f,  35.0f,  40.0f,  55.0f,  92.5f, 137.5f, 195.0f, 237.5f, 237.5f, 237.5f, 237.5f, 237.5f, 237.5f, 237.5f, 237.5f, 237.5f,
        040.0f,  47.5f,  60.0f,  72.5f, 140.0f, 160.0f, 217.5f, 245.0f, 245.0f, 245.0f, 245.0f, 245.0f, 245.0f, 245.0f, 245.0f, 245.0f,
        060.0f,  65.0f, 100.0f, 127.5f, 170.0f, 202.5f, 257.5f, 280.0f, 280.0f, 280.0f, 280.0f, 280.0f, 280.0f, 280.0f, 280.0f, 280.0f,
        100.0f, 145.0f, 142.5f, 217.5f, 272.5f, 260.0f, 302.5f, 250.0f, 250.0f, 250.0f, 250.0f, 250.0f, 250.0f, 250.0f, 250.0f, 250.0f,
        127.5f, 150.0f, 172.5f, 200.0f, 257.5f, 282.5f, 300.0f, 257.5f, 257.5f, 257.5f, 257.5f, 257.5f, 257.5f, 257.5f, 257.5f, 257.5f,
        152.5f, 137.5f, 140.0f, 155.0f, 192.5f, 230.0f, 252.5f, 247.5f, 247.5f, 247.5f, 247.5f, 247.5f, 247.5f, 247.5f, 247.5f, 247.5f,
        152.5f, 137.5f, 140.0f, 155.0f, 192.5f, 230.0f, 252.5f, 247.5f, 247.5f, 247.5f, 247.5f, 247.5f, 247.5f, 247.5f, 247.5f, 247.5f,
        152.5f, 137.5f, 140.0f, 155.0f, 192.5f, 230.0f, 252.5f, 247.5f, 247.5f, 247.5f, 247.5f, 247.5f, 247.5f, 247.5f, 247.5f, 247.5f,
        152.5f, 137.5f, 140.0f, 155.0f, 192.5f, 230.0f, 252.5f, 247.5f, 247.5f, 247.5f, 247.5f, 247.5f, 247.5f, 247.5f, 247.5f, 247.5f,
        152.5f, 137.5f, 140.0f, 155.0f, 192.5f, 230.0f, 252.5f, 247.5f, 247.5f, 247.5f, 247.5f, 247.5f, 247.5f, 247.5f, 247.5f, 247.5f,
        152.5f, 137.5f, 140.0f, 155.0f, 192.5f, 230.0f, 252.5f, 247.5f, 247.5f, 247.5f, 247.5f, 247.5f, 247.5f, 247.5f, 247.5f, 247.5f,
        152.5f, 137.5f, 140.0f, 155.0f, 192.5f, 230.0f, 252.5f, 247.5f, 247.5f, 247.5f, 247.5f, 247.5f, 247.5f, 247.5f, 247.5f, 247.5f,
        152.5f, 137.5f, 140.0f, 155.0f, 192.5f, 230.0f, 252.5f, 247.5f, 247.5f, 247.5f, 247.5f, 247.5f, 247.5f, 247.5f, 247.5f, 247.5f,
        152.5f, 137.5f, 140.0f, 155.0f, 192.5f, 230.0f, 252.5f, 247.5f, 247.5f, 247.5f, 247.5f, 247.5f, 247.5f, 247.5f, 247.5f, 247.5f))

  /** Coefficients matrix used for DCT transforms */
  private val coefficients = new Matrix[Float] {
    val width = 16
    val height = 16

    private val Double = 1 / 16f //sqrt(1 / 16) * sqrt(1 / 16)
    private val Single = 0.08838834764831845f //sqrt(1 / 16f) * sqrt(2 / 16f)
    private val Sans = 1 / 8f //sqrt(2 / 16f) * sqrt(2 / 16f)

    //Don't actually store the matrix; select values on the fly
    def apply(x: Int, y: Int) = if(x == 0 && y == 0)
      Double
    else if(x == 1 || y == 1)
      Single
    else
      Sans
  }

  /**
   * Pre-calculated discrete cosine matrix.
   * discreteCosine(u, v) selects parameters u and v, and
   * discreteCosine(u, v)(x, y) selects a cell x, y in the matrix with parameters u, v
   */
  private val discreteCosine: Matrix[Matrix[Float]] = {
    val result = new MutableArrayMatrix[Matrix[Float]](16, 16)
    val pi = 3.141592653589793f //Pi.toFloat

    var v = 0
    while(v < 16) {
      var u = 0
      while(u < 16) {
        val tmp = new MutableArrayMatrix[Float](16, 16)
        var y = 0
        while(y < 16) {
          var x = 0
          while(x < 16) {
            tmp(x, y) = (cos(pi * u * (2.0 * x + 1.0) / 32.0) *
                         cos(pi * v * (2.0 * y + 1.0) / 32.0)).toFloat
            x += 1
          }
          y += 1
        }
        result(u, v) = tmp
        u += 1
      }
      v += 1
    }
    result
  }

  /** Stretches the quantization matrix according to a quality value */
  @inline private def quant16Biased(quality: Int): Matrix[Float] = {
    //These magic numbers come from the standard JPEG algorithm
    val s = if(quality < 50) 5000 / quality else 200 - 2 * quality
    quant16 map (x => (s * x + 50) / 100)
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
  def partialDCT(input: Matrix[Float], size: Int): Matrix[Float] = {
    val w = input.width
    val h = input.height
    val result = new MutableArrayMatrix[Float](size, size)

    var v = 0
    while(v < size) {
      var u = 0
      while(u < size) {
        //Select parameters
        val localCosine = discreteCosine(u, v)
        var sum = 0f
        //Actually calculate the local sum
        var y = 0
        while(y < h) {
          var x = 0
          while(x < w) {
            sum += input(x, y) * localCosine(x, y)
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
  def makeBlocks(in: Matrix[Matrix[Float]]): Array[Block] = {
    status("Splitting up the image into DCT blocks with size " + partialDCTBlockSize)
    val quant = quant16Biased(quality)
    val res: Iterable[Block] = for((x, y, value) <- in.cells) yield {
      new Block(quantize(partialDCT(value, partialDCTBlockSize), quant), x, y, partialDCTBlockSize)
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
    val result = new ArrayBuffer[Shift](blocks.length / 16)
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
          for(s <- shifts) yield new Area(new Rectangle2D.Double(s.from.x, s.from.y, 16, 16))

        val rectsTo: Seq[Area] =
          for(s <- shifts) yield new Area(new Rectangle2D.Double(s.to.x, s.to.y, 16, 16))

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

  val result = input >- grayscale >- Fragmentize(16, 16) >- makeBlocks >- sortBlocks >-
  makeShifts >- groupShifts >- makeReport
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
