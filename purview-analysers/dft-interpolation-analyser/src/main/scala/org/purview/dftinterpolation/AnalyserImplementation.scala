package org.purview.dftinterpolation

import org.purview.core.analysis._
import org.purview.core.analysis.settings._
import org.purview.core.data._
import org.purview.core.transforms._
import org.purview.core.report.Warning
import scala.math._

class AnalyserImplementation extends HeatMapImageAnalyser with Settings {
  val name = "DFT Interpolation analyser"
  val description = "Finds interpolated regions in an image"
  override val version = Some("1.0")
  override val author = Some("Moritz Roth & David Flemström")

  override val message = "Interpolated region"
  override val reportLevel = Warning

  val grayscale = for(in <- input) yield in map (c => c.r + c.g + c.b)

  val nearest3 = grayscale >- Fragmentize(3, 1)

  val derivatives = for(in <- nearest3) yield
    in map (x => 2 * x(1, 0) - x(0, 0) - x(2, 0))

  val avg = for(d <- derivatives) yield {
    val a = d.sum / (d.width * d.height)
    d map ()
  }

  val fdt = avg >- FastFourierTransform2D()

  val heatmap = _
}
