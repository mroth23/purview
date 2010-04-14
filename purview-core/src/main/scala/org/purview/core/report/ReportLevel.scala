package org.purview.core.report

import java.io.Serializable
import org.purview.core.data.Color

sealed trait ReportLevel extends NotNull with Serializable {
  val name: String
}

trait LevelColor extends ReportLevel {
  def color: Color
}

object Debug extends ReportLevel
                with LevelColor {
  val name = "Debug"
  val color = Color(1, 0, 0.5f, 0)
}

object Information extends ReportLevel
                      with LevelColor {
  val name = "Information"
  val color = Color.Green
}

object Warning extends ReportLevel
                  with LevelColor {
  val name = "Warning"
  val color = Color.Yellow
}

object Error extends ReportLevel
                with LevelColor {
  val name = "Error"
  val color = Color.Red
}

object Critical extends ReportLevel
                   with LevelColor {
  val name = "Critical"
  val color = Color(1, 0, 0, 0)
}
