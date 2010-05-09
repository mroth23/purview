package org.purview.jpegghostanalyser

import org.purview.core.analysis._
import org.purview.core.analysis.settings._
import org.purview.core.data._
import org.purview.core.report._
import org.purview.core.transforms._
import scala.math._

class AnalyserImplementation extends Analyser[ImageMatrix] with Settings {
  val name = "JPEG Ghost Analyser"
  val description = "Finds JPEG ghosts in an image"

  //This will one day be an analyser based on the
  //"Exposing digital forgeries from JPEG Ghosts"
  //paper by Farid et al.

  val minQualitySetting = IntRangeSetting("Minimum quality", 1, 100)
  minQualitySetting.value = 50 //default
  val maxQualitySetting = IntRangeSetting("Maximum quality", 1, 100)
  maxQualitySetting.value = 100 //default
  val stepSizeSetting = IntRangeSetting("Step size", 1, 15)
  stepSizeSetting.value = 5 //default

  val settings = List(minQualitySetting, maxQualitySetting, stepSizeSetting)

  def result = for(in <- input) yield Set(new ReportMessage(Information, "Ran the analyser with the setting "))
}
