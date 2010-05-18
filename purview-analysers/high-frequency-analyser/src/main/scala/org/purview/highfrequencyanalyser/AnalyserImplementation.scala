package org.purview.highfrequencyanalyser

import org.purview.core.analysis._
import org.purview.core.analysis.settings._
import org.purview.core.data._
import org.purview.core.report._
import org.purview.core.transforms._
import scala.math._

class AnalyserImplementation extends Analyser[ImageMatrix] with Settings {
  val name = "High frequency analyser"
  val description = "Detects scaled regions by high-pass filtering"

  val frequencyWindow = IntRangeSetting("Size of frequency window", 1, 4)
  frequencyWindow.value = 3 //default

  val settings = List(frequencyWindow)

  private val window = frequencyWindow.value

  def grayscale(in: Matrix[Color]): Matrix[Float] = {
    status("Converting image to high-amplitude grayscale")
    in map (color => (color.redByte * 11 + color.greenByte * 16 + color.blueByte * 5) / 32 - 128f)
  }

  val blocks = input >- grayscale
}
