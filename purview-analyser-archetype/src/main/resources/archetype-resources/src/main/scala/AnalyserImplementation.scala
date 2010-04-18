package ${package}

import org.purview.core.analysis._
import org.purview.core.analysis.settings._
import org.purview.core.data._
import org.purview.core.report._
import org.purview.core.transforms._
import scala.math._

class AnalyserImplementation extends Analyser[ImageMatrix] with Settings {
  val name = "${artifactId}"
  val description = "The ${artifactId} analyser"

  val setting = IntRangeSetting("A setting", 1, 100)
  setting.value = 50 //default

  val settings = List(setting)

  def result = for(in <- input) yield Set(new ReportMessage(Information, "Ran the analyser with the setting " + setting.value))
}
