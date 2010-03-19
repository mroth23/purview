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
import org.purview.core.transforms.Fragmentize
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

  private sealed case class Block(values: Matrix[Float], x: Int, y: Int, size: Int) extends Ordered[Block] {
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

    private val Sixteenth = 0.0625f
    private val TwoSixteenths = Sixteenth * 2
    private val SqrtTwoOverSixteen = 0.08838834764831845f

    def apply(x: Int, y: Int) = if(x == 0 && y == 0)
      Sixteenth
    else if(x == 1 || y == 1)
      SqrtTwoOverSixteen
    else
      TwoSixteenths
  }

  private val discreteCosine: Matrix[Matrix[Float]] = {
    val result = new MutableArrayMatrix[Matrix[Float]](16, 16)
    val pi = Math.Pi.toFloat

    var y = 0
    while(y < 16) {
      var x = 0
      while(x < 16) {
        val tmp = new MutableArrayMatrix[Float](16, 16)
        var y0 = 0
        while(y0 < 16) {
          var x0 = 0
          while(x0 < 16) {
            tmp(x0, y0) = (Math.cos(x0 * pi * (x / 16f + 1f / 32f)) * Math.cos(pi * y0 * (y / 16f + 1f / 32f))).toFloat
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

  private def grayscale(in: Matrix[Color]): Matrix[Float] =
    in map (color => color.r * 0.3f + color.g * 0.59f + color.b * 0.11f - 0.5f)

  private def quantize(input: Matrix[Float], quant: Matrix[Float]): Matrix[Float] = {
    val w = input.width
    val h = input.height
    val result = new MutableArrayMatrix[Float](w, h)
    var y = 0
    while(y < h) {
      var x = 0
      while(x < w) {
        result(x, y) = Math.round(input(x, y) / quant(x, y))
        x += 1
      }
      y += 1
    }
    result
  }

  private def partiallyQuantizeDCT(input: Matrix[Float], quant: Matrix[Float],
                                   size: Int): Matrix[Float] = {
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
    quantize(result, quant)
  }

  private def makeBlocks(in: Matrix[Matrix[Float]]): Array[Block] = {
    val quant = quant16Biased(quality)
    val res: Iterable[Block] = for((x, y, value) <- in.cells) yield {
      new Block(partiallyQuantizeDCT(value, quant, partialDCTBlockSize), x, y, partialDCTBlockSize)
    }
    res.toArray
  }

  private def sortBlocks(blocks: Array[Block]) = {
    Sorting.quickSort(blocks)
    blocks
  }

  /*
   * Process:
   * - Convert image to grayscale
   * - Fragmentize the image into overlapping 16x16 pieces
   * - Make blocks out of these pieces (also quantizing them while we're at it)
   * - Sort the blocks with quickSort
   */

  def result = input >- grayscale >- Fragmentize(16, 16) >- makeBlocks >- sortBlocks
}
