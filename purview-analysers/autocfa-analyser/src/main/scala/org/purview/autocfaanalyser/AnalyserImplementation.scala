package org.purview.autocfaanalyser

import org.purview.core.analysis._
import org.purview.core.analysis.settings._
import org.purview.core.data._
import org.purview.core.report._
import org.purview.core.transforms._
import scala.math._
import scala.util.Sorting
class AnalyserImplementation extends Analyser[ImageMatrix] {
  /* This module is based on A.C. Gallagher's paper 'Image Authentication by Detecting Traces of Demosaicing'
   * It automatically determines if an image is computer-generated (PRCG) or photograhpic (PIM).
   * First, the green channel of the image is highpass-filtered, then the variance of every diagonal is computed.
   * The resulting function (one variance value for every diagonal) is then fourier-transformed.
   * If the image is real, there will be a large peak at a frequency of 0.5, and no significant other peaks.
   * Scaling can destroy or alter the peaks, and will also be detected by this analyser.
   * Many CG images aren't CFA interpolated and thus lack the typical periodic pattern. 
   *     */
  
  val name = "Automatic CFA analyser"
  val description = "Analyses the CFA pattern of the image"

  override val version = Some("1.0")
  override val author = Some("Moritz Roth & David Flemström")

  val extractGreen = for(matrix <- input) yield {
    for((x, y, color) <- matrix.cells) yield {
      color.g * 255f
    }
  }

  val highpassFilter = new ImmutableMatrix[Float](3, 3, Array(0, 1, 0, 1, -4, 1, 0, 1, 0))
  val filtered =  extractGreen >- Convolve(highpassFilter)

  val res = for(matrix <- filtered) yield {

    def getDiagonalVariances(w: Int, h: Int) : Array[Float] = {
      (for{
          x: Int <- 0 until w + h - 1
        } yield {

          val diag = (for{
              n: Int <- 0 until h
              if x - n >= 0 && x - n < w
            } yield {
              matrix(x - n, n).abs
            })
          //as described in the paper, we don't calculate the actual variance here, but the mean of the pixels in the diagonal
          (diag.sum / diag.length).toFloat
        }).toArray
    }

    def getDFTMagnitudes(diag: Seq[Float]) : Array[Float] = {
      val dft = new JTransforms1D(diag.length)
      var data = new Array[Float](diag.length * 2)
      for(i: Int <- 0 until diag.length){
        data(i * 2) = diag(i)
        data(i * 2 + 1) = 0f
      }
      var transformed = dft.FFT1DForward(data)
      var result = new Array[Float](diag.length)
      for(i: Int <- 0 until diag.length){
        result(i) = sqrt(transformed(i * 2) * transformed(i * 2) + transformed(i * 2 + 1) * transformed(i * 2 + 1)).toFloat
      }
      result
    }

    val magnitudes = getDFTMagnitudes(getDiagonalVariances(matrix.width, matrix.height))
    val sorted = magnitudes.clone
    Sorting.quickSort(sorted)

    val magMedian = sorted((sorted.length / 2).round)

    val result = magnitudes.tail.map(x => x / magMedian) //.tail excludes the DC value
    //result.foreach(x => println(x.round))

    (result, matrix)
  }

  def makeReport(x: (Array[Float],Matrix[Float])): Set[ReportEntry] =
    Set(new ReportMessage(Information, "The analyser ran successfully"), 
	new ReportImage(Information, "The highpass-filtered image", 0, 0, x._2.map(x => new Color(1, 0, (x / 255f), 0))))

  val result = res >- makeReport

}