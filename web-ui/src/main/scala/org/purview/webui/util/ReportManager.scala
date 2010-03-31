package org.purview.webui.util

import net.liftweb.common.Logger
import net.liftweb.util.Helpers
import org.purview.core.analysis.Metadata
import org.purview.core.report.ReportEntry
import scala.collection.mutable

object ReportManager extends Logger {
  //TODO: actually persist reports!
  private val reports: mutable.Map[String, Map[Metadata, Set[ReportEntry]]] = mutable.Map.empty
  def loadReport(id: String): Map[Metadata, Set[ReportEntry]] =
    reports.get(id) getOrElse Map.empty

  def reportExists(id: String): Boolean = reports contains id
  
  def makeId = Helpers.randomString(16)

  def saveReport(report: Map[Metadata, Set[ReportEntry]], id: String = makeId): Option[String] = {
    reports(id) = report
    Some(id)
  }
}
