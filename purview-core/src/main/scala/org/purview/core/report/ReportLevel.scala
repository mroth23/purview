package org.purview.core.report

import org.purview.core.data.Color
import org.purview.core.data.Colors

trait ReportLevel extends NotNull {
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
  val color = Colors.Green
}

object Warning extends ReportLevel
                  with LevelColor {
  val name = "Warning"
  val color = Colors.Yellow
}

object Error extends ReportLevel
                with LevelColor {
  val name = "Error"
  val color = Colors.Red
}

object Critical extends ReportLevel
                   with LevelColor {
  val name = "Critical"
  val color = Color(1, 0, 0, 0)
}
