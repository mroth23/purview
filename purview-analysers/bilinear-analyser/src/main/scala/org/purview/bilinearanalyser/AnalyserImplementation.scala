package org.purview.bilinearanalyser

import org.purview.core.analysis._
import org.purview.core.analysis.settings._
import org.purview.core.data._
import org.purview.core.transforms.Convolve
import org.purview.core.report.Warning
import org.purview.core.transforms.FastFourierTransform1D
import scala.math._

class AnalyserImplementation extends HeatMapImageAnalyser{
  val name = "Bilinear analyser"
  val description = "Finds bilinearly interpolated regions in an image"
  override val version = Some("1.3")
  override val author = Some("Moritz Roth & David Flemström")

  override val iconResource = Some("icons/analysers/bilinear.png")

  override val message = "Interpolated region"
  override val reportLevel = Warning

  val extractGreen = for(matrix <- input) yield {
    for((x, y, color) <- matrix.cells) yield {
      color.g
    }
  }

  val highpassFilter = new ImmutableMatrix[Float](3,3, Array(0, 1, 0, 1, -4, 1, 0, 1, 0))
  val hpf =  extractGreen >- Convolve(highpassFilter)

  val res = for(matrix <- hpf) yield {
    status("Calculating diagonal variances")
    
    def getDiagonalVariance(xStart: Int, yStart: Int, n: Int) : Float = {
      val x = xStart
      val y  = yStart
      val sum = (for(i: Int <- -n to n) yield (matrix(x + i, y + i).abs)).sum.toFloat
      sum / (2 * n + 1).toFloat
    }

//    def getDFT(diag: Seq[Float]) : Seq[Complex[Float]] = {
//      val dft = FastFourierTransform1D()
//      dft(diag.map(x => new Complex(x, 0)))
//    }

    val varianceMatrix = (for(i : Int <- 0 until matrix.width; j : Int <- 0 until matrix.height) yield (
        getDiagonalVariance(i, j, 32)))

    val result = matrix.cells.map {cell =>
      val (x, y, cellValue) = cell
      getDiagonalVariance(x, y, 32)
    }

    result
  }

  val heatmap = res
}
