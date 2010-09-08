package org.purview.bilinearanalyser

import org.purview.core.analysis._
import org.purview.core.analysis.settings._
import org.purview.core.data._
import org.purview.core.report.Warning
import org.purview.core.transforms._
import scala.math._

class AnalyserImplementation extends HeatMapImageAnalyser{
  val name = "Bilinear analyser"
  val description = "Finds bilinearly interpolated regions in an image"
  override val version = Some("1.3")
  override val author = Some("Moritz Roth & David Flemström")

  override val iconResource = Some("icons/analysers/bilinear.png")

  override val message = "Interpolated region"
  override val reportLevel = Warning


  //This is broken and purely experimental.
  val extractGreen = for(matrix <- input) yield {
    for((x, y, color) <- matrix.cells) yield {
      color.g
    }
  }

  val highpassFilter = new ImmutableMatrix[Float](3, 3, Array(0, 1, 0, 1, -4, 1, 0, 1, 0))
  val hpf =  extractGreen >- Convolve(highpassFilter)

  val res = for(matrix <- hpf) yield {
    status("Calculating diagonal variances")
    
    def getDiagonalVariance(xStart: Int, yStart: Int, n: Int) : Float = {
      val x = xStart
      val y  = yStart
      val sum = (for(i: Int <- -n to n) yield getMatrixValue(x,y)).sum.toFloat
      sum / (2 * n + 1).toFloat
    }

    def getMatrixValue(x: Int, y: Int): Float = {
      val xr = floor(x.toFloat / matrix.width.toFloat).abs.toInt
      val yr = floor(y.toFloat / matrix.height.toFloat).abs.toInt

      matrix((x - xr * matrix.width).abs, (y - yr * matrix.width).abs).abs
    }

    def getDFT(diag: Seq[Float]) : Seq[Complex[Float]] = {
      val dft = new JTransforms1D(diag.length)
	var data = new Array[Float](diag.length * 2)
	for(i: Int <- 0 to diag.length){
	  data(i * 2) = diag(i)
	  data(i * 2 + 1) = 0f
	}
      var transformed = dft.DCT1DForward(data, true)
	var result = new Array[Complex[Float]](diag.length)
	for(i: Int <- 0 to transformed.length){
	  result(i) = new Complex(transformed(i * 2), transformed(i * 2 + 1))
	}
	result
    }

    val varianceMatrix = (for(i : Int <- 0 to matrix.width; j : Int <- 0 to matrix.height) yield (
        getDiagonalVariance(i, j, 32)))

    val result = matrix.cells.map {cell =>
      val (x, y, cellValue) = cell
      getDiagonalVariance(x, y, 32)
    }

    result
  }

  val heatmap = res
}
