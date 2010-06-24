package org.purview.autocfaanalyser

import org.purview.core.analysis._
import org.purview.core.analysis.settings._
import org.purview.core.data._
import org.purview.core.report._
import org.purview.core.transforms._
import scala.math._

class AnalyserImplementation extends Analyser[ImageMatrix] {
  /* This module is based on A.C. Gallagher's paper 'Image Authentication by Detecting Traces of Demosaicing'
   * It automatically determines if an image is computer-generated (PRCG) or photograhpic (PIM).
   * First, the green channel of the image is highpass-filtered, then the variance of every diagonal is computed.
   * This yields a discrete signal in space-domain (one value for every diagonal), which is fourier-transformed.
   * If the image is real, there will be a large peak at a frequency of 0.5, and no significant other peaks.
   * Scaling can destroy or alter the peaks, and will also be detected by this analyser.
   * Many CG images aren't CFA interpolated and thus lack the typical periodic pattern. 
   *     */
  val name = "Automatic CFA analyser"
  val description = "Determines if an image is CGI by analysing the CFA pattern"

  override val version = Some("1.0")
  override val author = Some("Moritz Roth & David Flemström")

  val extractGreen = for(matrix <- input) yield {
    for((x, y, color) <- matrix.cells) yield {
      color.g
    }
  }

  val highpassFilter = new ImmutableMatrix[Float](3, 3, Array(0, 1, 0, 1, -4, 1, 0, 1, 0))
  val filtered =  extractGreen >- Convolve(highpassFilter)

  val res = for(matrix <- filtered) yield {

    def getDiagonalVariances(w: Int, h: Int) : Array[Float] = {
	  (for{
		  x: Int <- 0 to w + h
		} yield {

		  val diag = (for{
			  n: Int <- 0 to h
			  if x - n >= 0 && x - n <= w
			} yield {
			  matrix(x - n, n).abs
			})
		  //as described in the paper, we don't calculate the actual variance here, but the mean of the pixels in the diagonal
		  (diag.sum / diag.length).toFloat
		}).toArray
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

	val magnitudes = getDFT(getDiagonalVariances(matrix.width, matrix.height)).map(x => sqrt(x.real * x.real + x.imag * x.imag).toFloat)
	val magMean = (magnitudes.sum - magnitudes(0)) / (magnitudes.length - 1f)

	val result = magnitudes.map(x => x - magMean)
	result(0)
  }

  def makeReport(value: Float): Set[ReportEntry] =
    Set(new ReportMessage(Information, "The analyser ran successfully :O Value 0 is " + value.toString))

  val result = res >- makeReport

}