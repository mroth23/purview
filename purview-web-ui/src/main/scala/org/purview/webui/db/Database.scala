package org.purview.webui.db

import org.squeryl.Schema

object Database extends Schema {
  val analyses = table[Analysis]("analyses")
  val reports = table[AnalyserReport]("reports")
  val reportEntryFiles = table[AnalyserReportEntryFile]("report_entries")
}
