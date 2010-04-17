package org.purview.webui.db

import org.squeryl.Schema

object Database extends Schema {
  val analyses = table[Analysis]("analyses")
}
