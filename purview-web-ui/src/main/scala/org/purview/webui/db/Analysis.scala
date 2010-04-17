package org.purview.webui.db

import org.squeryl.KeyedEntity
import org.squeryl.PrimitiveTypeMode._

sealed case class Analysis(fileName: String, handle: String) extends KeyedEntity[Long] {
  val id: Long = 0
}
