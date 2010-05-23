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
    status("Calculating diagonal variance")
    
    def getDiagonalVariance(xStart: Int, yMax: Int) : Float = {
      var sum = 0f
      var x = xStart
      var y  = 0
      var nd = 0
      while(y < yMax && x >= 0)
      {
        sum += matrix(x,y)
        x -= 1
        y += 1
        nd += 1
      }
      sum / nd.toFloat
    }

    def getDFT(diag: Seq[Float]) : Seq[Complex[Float]] = {
      val dft = FastFourierTransform1D()
      dft(diag.map(x => new Complex(x, 0)))
    }

    

    val result = matrix
    result: Matrix[Float]
  }

  val heatmap = hpf
}
