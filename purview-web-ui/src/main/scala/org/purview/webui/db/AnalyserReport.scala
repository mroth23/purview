package org.purview.webui.db

import org.purview.core.analysis.Metadata
import org.squeryl.KeyedEntity
import org.squeryl.PrimitiveTypeMode._

sealed case class AnalyserReport(analysisId: Long,
                                 name: String, description: String,
                                 override val version: Option[String], override val author: Option[String],
                                 override val iconResource: Option[String]) extends KeyedEntity[Long] with Metadata {
  val id = 0l
  def entries = Database.reportEntryFiles.where(_.reportId === id)
  def this() = this(0, "", "", Some(""), Some(""), Some(""))
}

sealed case class AnalyserReportEntryFile(reportId: Long, fileKey: String)
