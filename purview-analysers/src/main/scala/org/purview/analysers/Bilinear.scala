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
import org.purview.core.process.Stage
import org.purview.core.transforms.Convolve
import scala.math._

class Bilinear extends HeatMapImageAnalyser with Metadata with Settings {
  val name = "Bilinear"
  val description = "Finds bilinearly interpolated regions in an image"

  override val message = "Bilinearly scaled region"

  val maxSizeFactorSetting = IntRangeSetting("Max factor to detect", 2, 16)
  maxSizeFactorSetting.value = 8 //default
  
  val epsilonSetting = FloatRangeSetting("Leniance epsilon value", 1f/255f, 1f)

  val settings = List(maxSizeFactorSetting, epsilonSetting)

  //Just to make us have to type less
  def maxSizeFactor = maxSizeFactorSetting.value
  def epsilon = epsilonSetting.value

  val markBilinear = Stage("Mark bilinear image regions") {
    for(in <- input) yield {
      val result = new MutableMatrix[Float](in.width, in.height)
      val accessors = List[(Int, Int, Int) => Color](
        (x, y, n) => in(min(x + n, in.width - 1), y                        ),
        (x, y, n) => in(x                       , min(y + n, in.height - 1)),
        (x, y, n) => in(min(x + n, in.width - 1), min(y + n, in.height - 1))
      )

      val w = in.width
      val h = in.height

      for {
        (x, y, color) <- in.cells
        read <- accessors
      } {
        val streak = for(extend <- 1 to 16) yield read(x, y, extend)
        val slopes = streak.sliding(2).map(x => x(0) - x(1)).toSeq
        val first = slopes.head
        val firstabs = first.abs

        if(firstabs.a > epsilon || firstabs.r > epsilon ||
           firstabs.g > epsilon || firstabs.b > epsilon) {
          val numberOfSlopes = slopes.findIndexOf { x =>
            val xabs = (x - first).abs
            !(xabs.a < epsilon && xabs.r < epsilon && xabs.g < epsilon && xabs.b < epsilon)
          }

          if(numberOfSlopes > -1)
            result(x, y) = numberOfSlopes
        }
      }
      result
    }
  }

  private val gaussian5BlurKernel =
    new ImmutableMatrix(9, 9,
                        Array[Float](0.0016f, 0.0032f, 0.0048f, 0.0064f, 0.0080f, 0.0064f, 0.0048f, 0.0032f, 0.0016f,
                                     0.0032f, 0.0064f, 0.0096f, 0.0128f, 0.016f , 0.0128f, 0.0096f, 0.0064f, 0.0032f,
                                     0.0048f, 0.0096f, 0.0144f, 0.0192f, 0.024f , 0.0192f, 0.0144f, 0.0096f, 0.0048f,
                                     0.0064f, 0.0128f, 0.0192f, 0.0256f, 0.032f , 0.0256f, 0.0192f, 0.0128f, 0.0064f,
                                     0.0080f, 0.016f , 0.024f , 0.032f , 0.04f  , 0.032f ,  0.024f, 0.016f , 0.0080f,
                                     0.0064f, 0.0128f, 0.0192f, 0.0256f, 0.032f , 0.0256f, 0.0192f, 0.0128f, 0.0064f,
                                     0.0048f, 0.0096f, 0.0144f, 0.0192f, 0.024f , 0.0192f, 0.0144f, 0.0096f, 0.0048f,
                                     0.0032f, 0.0064f, 0.0096f, 0.0128f, 0.016f , 0.0128f, 0.0096f, 0.0064f, 0.0032f,
                                     0.0016f, 0.0032f, 0.0048f, 0.0064f, 0.0080f, 0.0064f, 0.0048f, 0.0032f, 0.0016f))

  val blur = Stage("Computing blurred bilinear image regions")(markBilinear >- Convolve(gaussian5BlurKernel))

  val heatmap = blur
}
