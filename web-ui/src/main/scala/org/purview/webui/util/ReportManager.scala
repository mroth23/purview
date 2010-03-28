package org.purview.webui.util

import net.liftweb.common.Logger
import net.liftweb.util.Helpers
import org.purview.core.analysis.Metadata
import org.purview.core.report.ReportEntry
import org.purview.core.report.ReportPersistance
import scala.xml.XML

object ReportManager extends FileManager with Logger {
  def loadReport(id: String): Map[Metadata, Set[ReportEntry]] = {
    val file = createFile(id)
    if(file.exists) {
      try {
        val xml = XML.loadFile(file)
        ReportPersistance.decodeReportTree(xml)
      } catch {
        case ex => 
          info("Error when loading report file: \n" + ex.getStackTraceString)
          Map.empty
      }
    } else {
      info("Couldn't find report file " + file)
      Map.empty
    }
  }

  def reportExists(id: String): Boolean = createFile(id).exists
  
  def makeId = Helpers.randomString(16)

  def saveReport(report: Map[Metadata, Set[ReportEntry]], id: String = makeId): Option[String] = {
    val file = createFile(id)
    try {
      XML.save(file.getAbsolutePath, ReportPersistance.encodeReportTree(report), "UTF-8", true, null)
      Some(id)
    } catch {
      case ex =>
        info("Error when saving report file: \n" + ex.getStackTraceString)
        None
    }
  }
}
