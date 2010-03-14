package org.purview.analysers

import org.purview.core.analysis.HeatMapImageAnalyser
import org.purview.core.analysis.Metadata
import org.purview.core.analysis.Settings
import org.purview.core.analysis.settings.IntRangeSetting
import org.purview.core.analysis.settings.FloatRangeSetting
import org.purview.core.data.Color
import org.purview.core.data.ImmutableMatrix
import org.purview.core.data.Matrix
import org.purview.core.data.MutableMatrix
import org.purview.core.report.Warning
import org.purview.core.transforms.LinearConvolve
import scala.math._

class Bilinear extends HeatMapImageAnalyser with Metadata with Settings {
  val name = "Bilinear"
  val description = "Finds bilinearly interpolated regions in an image"

  override val message = "Bilinearly scaled region"
  override val reportLevel = Warning
  override def threshold = 1.0f

  val maxSizeFactorSetting = IntRangeSetting("Max factor to detect", 2, 16)
  maxSizeFactorSetting.value = 8 //default
  
  val epsilonSetting = FloatRangeSetting("Leniance epsilon value", 1f/255f, 1f)

  val settings = List(maxSizeFactorSetting, epsilonSetting)

  //Just to make us have to type less
  private def maxSizeFactor = maxSizeFactorSetting.value
  private def epsilon = epsilonSetting.value

  def markBilinear = for(matrix <- input) yield {
    status("Marking bilinearly scaled image regions")
    val range = (1 to maxSizeFactor).toSeq
    val width = matrix.width
    val height = matrix.height
    @inline def cmpSlope(slope1: Color, slope2: Color) = {
      abs(slope1.a - slope2.a) < epsilon && abs(slope1.r - slope2.r) < epsilon &&
      abs(slope1.g - slope2.g) < epsilon && abs(slope1.b - slope2.b) < epsilon
    }
    for {
      (x, y, color) <- matrix.cells
    } yield if(x == width - 1 || y == height - 1)
      0.0f
    else {
      val slopeRight = matrix(x + 1, y) - matrix(x, y)
      val slopeDown = matrix(x, y + 1) - matrix(x, y)
      val slopeDiag = matrix(x + 1, y + 1) - matrix(x, y)
      if(slopeRight.weight > epsilon && slopeDown.weight > epsilon && slopeDiag.weight > epsilon) {
        val consecutiveRight = range.findLastIndexOf { extend =>
          if(x + extend < width) {
            val tmpSlope = matrix(x + extend, y) - matrix(x + extend - 1, y)
            cmpSlope(tmpSlope, slopeRight)
          } else false
        }
        val consecutiveDown = range.findLastIndexOf { extend =>
          if(y + extend < height) {
            val tmpSlope = matrix(x, y + extend) - matrix(x, y + extend - 1)
            cmpSlope(tmpSlope, slopeDown)
          } else false
        }

        val consecutiveDiag = range.findLastIndexOf { extend =>
          if(y + extend < height && x + extend < width) {
            val tmpSlope = matrix(x + extend, y + extend) - matrix(x + extend - 1, y + extend - 1)
            cmpSlope(tmpSlope, slopeDiag)
          } else false
        }
        (consecutiveRight + consecutiveDown + consecutiveDiag).toFloat
      } else 0f
    }
  }

  private val gaussian5BlurKernel = Array[Float](0.0080f, 0.016f, 0.024f, 0.032f, 0.04f, 0.032f,  0.024f, 0.016f, 0.0080f)

  def heatmap = markBilinear >- LinearConvolve(gaussian5BlurKernel)
}
