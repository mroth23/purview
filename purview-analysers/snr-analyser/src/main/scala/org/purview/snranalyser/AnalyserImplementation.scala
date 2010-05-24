package org.purview.snranalyser

import org.purview.core.analysis._
import org.purview.core.analysis.settings._
import org.purview.core.data._
import org.purview.core.report._
import org.purview.core.transforms._
import scala.math._

class AnalyserImplementation extends HeatMapImageAnalyser{
  val name = "Signal-to-noise ratio analyser"
  val description = "Analyses the signal-to-noise ratio in the image"
  override val colored = false

  val grayscale: Computation[Matrix[Float]] = for (in <- input) yield (in.map(color => (color.r * 11f + color.g * 16f + color.b * 5f) / 32f))

  def mean(matrix: Matrix[Float]): Float = matrix.sum.toFloat / (matrix.width * matrix.height).toFloat

  val meanGSC = for(gsc <- grayscale) yield (mean(gsc).toFloat)

  val deviation = for(gsc <- grayscale; m <- meanGSC) yield (gsc map (_.toFloat - m.toFloat))

  val snr = for(m <- meanGSC; dev <- deviation) yield (dev map (x => m.toFloat / x.toFloat))

  val heatmap = snr
}
