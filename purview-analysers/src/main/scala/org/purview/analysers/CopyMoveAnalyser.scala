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

class CopyMoveAnalyser extends Analyser[ImageMatrix] with Metadata with Settings {
  val name = "Copy move analyser"
  val description = ""

  val result = error("TODO")

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

  private def quant16Biased(quality: Int): Matrix[Float] = {
    val s = if(quality < 50) 5000 / quality else 200 - 2 * quality
    val quant8Biased = quant8 map (x => (s * x + 50) / 100)

    val result = new MutableArrayMatrix[Float](16, 16);
    var x = 0
    while(x < 16) {
      var y = 0
      while(y < 16) {
        if(x == 0 && y == 0)
          result(x, y) = quant8Biased(x, y) * 2
        else if(x < 8 && y < 8)
          result(x, y) = quant8Biased(x, y) * 2.5f
        else
          //OPTIMIZATION: we know that (0, 7) == (7, 7) == (7, 0)
          result(x, y) = quant8Biased(7, 7) * 2.5f
        y += 1
      }
      x += 1
    }
    result
  }

  @inline private def lum(color: Color) =
    color.r * 0.3f + color.g * 0.59f + color.b * 0.11f - 0.5f

  def make = for(in <- input) yield {
    val quant16 = quant16Biased(quality)
  }
}
