package org.purview.analysers

import org.purview.core.analysis.HeatMapImageAnalyser
import org.purview.core.analysis.Metadata
import org.purview.core.data.Color
import org.purview.core.data.ImmutableMatrix
import org.purview.core.data.Matrix
import org.purview.core.data.MutableMatrix
import org.purview.core.stage.stage
import org.purview.stages.Convolve

class Bilinear extends HeatMapImageAnalyser with Metadata {
  val name = "Bilinear"
  val description = "Finds bilinearly interpolated regions in an image"

  override val message = "Bilinearly scaled region"

  val MaxSizeFactor = 8
  val Epsilon = 1f/255f

  val markBilinear = stage("Mark bilinear image regions") { input: Matrix[Color] =>
    val result = new MutableMatrix[Float](input.width, input.height)
    val accessors = List[(Int, Int, Int) => Color](
      (x, y, n) => input(Math.max(x + n, input.width), y                            ),
      (x, y, n) => input(x                           , Math.max(y + n, input.height)),
      (x, y, n) => input(Math.max(x + n, input.width), Math.max(y + n, input.height))
    )

    for {
      (x, y, color) <- input.cells
      read <- accessors
    } {
      val streak = for(extend <- 1 to 16) yield read(x, y, extend)
      val slopes = streak.sliding(2).map(x => x(0) - x(1)).toSeq
      val first = slopes.head
      val firstabs = first.abs

      if(firstabs.a > Epsilon || firstabs.r > Epsilon ||
         firstabs.g > Epsilon || firstabs.b > Epsilon) {
        val numberOfSlopes = slopes.findIndexOf { x =>
          val xabs = (x - first).abs
          !(xabs.a < Epsilon && xabs.r < Epsilon && xabs.g < Epsilon && xabs.b < Epsilon)
        }

        if(numberOfSlopes > -1)
          result(x, y) = numberOfSlopes
      }
    }
    result
  }

  private val gaussian5BlurKernel = new ImmutableMatrix(9, 9,
    Array[Float](0.0016f, 0.0032f, 0.0048f, 0.0064f, 0.0080f, 0.0064f, 0.0048f, 0.0032f, 0.0016f,
                 0.0032f, 0.0064f, 0.0096f, 0.0128f, 0.016f , 0.0128f, 0.0096f, 0.0064f, 0.0032f,
                 0.0048f, 0.0096f, 0.0144f, 0.0192f, 0.024f , 0.0192f, 0.0144f, 0.0096f, 0.0048f,
                 0.0064f, 0.0128f, 0.0192f, 0.0256f, 0.032f , 0.0256f, 0.0192f, 0.0128f, 0.0064f,
                 0.0080f, 0.016f , 0.024f , 0.032f , 0.04f  , 0.032f ,  0.024f, 0.016f , 0.0080f,
                 0.0064f, 0.0128f, 0.0192f, 0.0256f, 0.032f , 0.0256f, 0.0192f, 0.0128f, 0.0064f,
                 0.0048f, 0.0096f, 0.0144f, 0.0192f, 0.024f , 0.0192f, 0.0144f, 0.0096f, 0.0048f,
                 0.0032f, 0.0064f, 0.0096f, 0.0128f, 0.016f , 0.0128f, 0.0096f, 0.0064f, 0.0032f,
                 0.0016f, 0.0032f, 0.0048f, 0.0064f, 0.0080f, 0.0064f, 0.0048f, 0.0032f, 0.0016f))

  val heatmap = markBilinear andThen Convolve(gaussian5BlurKernel)
}
