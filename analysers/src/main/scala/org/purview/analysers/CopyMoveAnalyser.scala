package org.purview.analysers

import org.purview.core.analysis.Analyser
import org.purview.core.analysis.Metadata
import org.purview.core.analysis.Settings
import org.purview.core.analysis.settings.IntRangeSetting
import org.purview.core.data.Color
import org.purview.core.data.ImageMatrix
import org.purview.core.data.ImmutableMatrix
import org.purview.core.data.Matrix
import org.purview.core.data.MutableArrayMatrix
import org.purview.core.report.Critical
import org.purview.core.report.ReportEntry
import org.purview.core.report.ReportRectangleMove
import org.purview.core.transforms.Fragmentize
import scala.collection.mutable.ArrayBuffer
import scala.math._
import scala.util.Sorting

class CopyMoveAnalyser extends Analyser[ImageMatrix] with Metadata with Settings {
  val name = "Copy-Move"
  val description = "Finds cloned regions in an image"

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

  private def quality = qualitySetting.value
  private def threshold = thresholdSetting.value
  private def minDistance = minDistanceSetting.value
  private def partialDCTBlockSize = partialDCTBlockSizeSetting.value

  sealed case class Block(values: Matrix[Float], x: Int, y: Int, size: Int) extends Ordered[Block] {
    require(values.width == size && values.height == size, "Block with invalid value matrix created!")
    
    def compare(that: Block): Int = {
      val n = if(this.size < that.size) this.size else that.size

      var y = 0
      while(y < n) {
        var x = 0
        while(x < n) {
          val a = this.values(x, y)
          val b = that.values(x, y)

          //Please forgive me for using "return"...
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

    override def hashCode = values.foldLeft(0)(_ ^ _.hashCode)
  }

  sealed case class Shift(from: Block, to: Block) {
    val lengthSquared = (from.x - to.x) * (from.x - to.x) + (from.y - to.y) * (from.y - to.y)
    val vector = {
      val x1 = from.x - to.x
      val y1 = from.y - to.y
      if(x1 < 0) (-x1, -y1) else (x1, y1)
    }
  }

  private val quant8 =
    ImmutableMatrix[Int](8, 8, Array(
        4,  4,   6, 11, 24, 24, 24, 24,
        4,  5,   6, 16, 24, 24, 24, 24,
        6,  6,  14, 24, 24, 24, 24, 24,
        11, 16, 24, 24, 24, 24, 24, 24,
        24, 24, 24, 24, 24, 24, 24, 24,
        24, 24, 24, 24, 24, 24, 24, 24,
        24, 24, 24, 24, 24, 24, 24, 24,
        24, 24, 24, 24, 24, 24, 24, 24
      ))

  private val quant16 =
    ImmutableMatrix[Float](16, 16, Array(
        08.0f, 10.0f, 15.0f, 27.5f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f,
        10.0f, 12.5f, 15.0f, 40.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f,
        15.0f, 15.0f, 35.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f,
        27.5f, 40.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f,
        60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f,
        60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f,
        60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f,
        60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f,
        60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f,
        60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f,
        60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f,
        60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f,
        60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f,
        60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f,
        60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f,
        60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f, 60.0f
      ))

  private val coefficients = new Matrix[Float] {
    val width = 16
    val height = 16

    private val Double = 1 / 16f //sqrt(1 / 16) * sqrt(1 / 16)
    private val Single = 0.08838834764831845f //sqrt(1 / 16f) * sqrt(2 / 16f)
    private val Sans = 1 / 8f //sqrt(2 / 16f) * sqrt(2 / 16f)

    def apply(x: Int, y: Int) = if(x == 0 && y == 0)
      Double
    else if(x == 1 || y == 1)
      Single
    else
      Sans
  }

  private val discreteCosine: Matrix[Matrix[Float]] = {
    val result = new MutableArrayMatrix[Matrix[Float]](16, 16)
    val pi = 3.141592653589793f //Pi.toFloat

    var y = 0
    while(y < 16) {
      var x = 0
      while(x < 16) {
        val tmp = new MutableArrayMatrix[Float](16, 16)
        var y0 = 0
        while(y0 < 16) {
          var x0 = 0
          while(x0 < 16) {
            tmp(x0, y0) = (cos(pi * x * (2.0 * x0 + 1.0) / 32.0) * cos(pi * y * (2.0 * y0 + 1.0) / 32.0)).toFloat
            x0 += 1
          }
          y0 += 1
        }
        result(x, y) = tmp
        x += 1
      }
      y += 1
    }
    result
  }

  @inline private def quant16Biased(quality: Int): Matrix[Float] = {
    val s = if(quality < 50) 5000 / quality else 200 - 2 * quality
    quant16 map (x => (s * x + 50) / 100)
  }

  def grayscale(in: Matrix[Color]): Matrix[Float] = {
    status("Converting image to high-amplitude grayscale")
    in map (color => (color.r * 11 + color.g * 16 + color.b * 5) / 32 - 128)
  }

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
       with agenda items
      y += 1
    }
    result
  }

  def partialDCT(input: Matrix[Float], size: Int): Matrix[Float] = {
    val w = input.width
    val h = input.height
    val result = new MutableArrayMatrix[Float](size, size)

    var y = 0
    while(y < size) {
      var x = 0
      while(x < size) {
        val localCosine = discreteCosine(x, y)
        var sum = 0f
        var y0 = 0
        while(y0 < h) {
          var x0 = 0
          while(x0 < w) {
            sum += input(x0, y0) * localCosine(x0, y0)
            x0 += 1
          }
          y0 += 1
        }
        sum *= coefficients(x, y)
        result(x, y) = sum
        x += 1
      }
      y += 1
    }
    result
  }

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
    var i = 0
    while(i < blocks.length - 1) {
      val block1 = blocks(i)
      val block2 = blocks(i + 1)
      progress(i.toFloat / blocks.length)
      if((block1 compare block2) == 0) {
        val shift = if(block1.x < block2.x)
          new Shift(block1, block2)
        else if(block1.x == block2.x && block1.y < block2.y)
          new Shift(block1, block2)
        else
          new Shift(block2, block1)
        
        if(shift.lengthSquared > minimumDistanceSquared)
          result += shift
      }
      i += 1
    }
    result
  }

  def groupShifts(shifts: Seq[Shift]): Map[(Int, Int), Seq[Shift]] = {
    status("Grouping shifts by displacement vector")
    shifts.groupBy(_.vector).toMap
  }

  def makeReport(groupedShifts: Map[(Int, Int), Seq[Shift]]): Set[ReportEntry] =
    (for((vector, shifts) <- groupedShifts if shifts.length > threshold) yield {
        var left = 0
        var top = 0
        var right = 0
        var bottom = 0

        for(shift <- shifts; s = shift.from) {
          if(s.x < left)
            left = s.x
          if(s.x + s.size > right)
            right = s.x + s.size
          if(s.y < top)
            top = s.y
          if(s.y + s.size > bottom)
            bottom = s.y + s.size
        }
        val x = (left + right) / 2
        val y = (top + bottom) / 2

        new ReportRectangleMove(Critical, "This region was moved", x, y, x + vector._1, y + vector._2, right - left, bottom - top)
      }).toSet

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

  def result = input >- grayscale >- Fragmentize(16, 16) >- makeBlocks >- sortBlocks >-
  makeShifts >- groupShifts >- makeReport
}
