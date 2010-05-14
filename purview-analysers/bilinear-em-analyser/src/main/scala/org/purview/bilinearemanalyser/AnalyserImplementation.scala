package org.purview.bilinearemanalyser

import org.purview.core.analysis._
import org.purview.core.analysis.settings._
import org.purview.core.data._
import org.purview.core.report.Warning
import scala.math._

class AnalyserImplementation extends HeatMapImageAnalyser with Settings {
  val name = "Bilinear EM analyser"
  val description = "Finds bilinearly interpolated regions in an image"
  override val version = Some("1.0")
  override val author = Some("Moritz Roth & David Flemström")

  override val message = "Bilinearly scaled region"
  override val reportLevel = Warning

  val N = 2
  val sigma = 0.0075

  val markBilinear = for(matrix <- input) yield {
    status("Performing an expectation-maximization transformation")

    val a = new MutableArrayMatrix[Float](N, N)
  }

  private val gaussian5BlurKernel = Array[Float](0.0080f, 0.016f, 0.024f, 0.032f, 0.04f, 0.032f,  0.024f, 0.016f, 0.0080f)

  override val convolve: Computation[Option[Array[Float]]] = Computation(Some(gaussian5BlurKernel))

  val heatmap = markBilinear
}
