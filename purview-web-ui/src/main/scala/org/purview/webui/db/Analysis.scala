package org.purview.webui.db

import org.squeryl.KeyedEntity
import org.squeryl.PrimitiveTypeMode._

sealed case class Analysis(fileName: String,
                           originalImageKey: String,
                           optimizedImageKey: String,
                           scaledImageKey: String) extends KeyedEntity[Long] {
  val id: Long = 0
  def analyserReports = Database.reports.where(_.analysisId === id)
}
