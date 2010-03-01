package org.purview.core.analysis

import org.purview.core.analysis.settings.Setting
import org.purview.core.report.ReportEntry
import org.purview.core.session.AnalysisStats
import scala.util.DynamicVariable

object Analyser {
  private[core] val statsVar = new DynamicVariable[AnalysisStats](new AnalysisStats)
  def statistics = statsVar.value
  def analyseWithStats[A](analyser: Analyser[A], what: A)(implicit s: AnalysisStats) = statsVar.withValue(s) {
    analyser.analyse(what)
  }
}

/**
 * An object that processes something and generates a report.
 */
abstract class Analyser[A] extends NotNull {
  /** Runs this Analyser and generates a report */
  def analyse(what: A): Set[ReportEntry]

  def status(status: String) = Analyser.statistics.reportStatus(status)
  def progress(progress: Float) = Analyser.statistics.reportProgress(progress)
}

trait Metadata {
  def name: String
  def description: String
  def version: String = ""
  def author: String = ""
}

trait Settings {
  val settings: Seq[Setting[_]]
}