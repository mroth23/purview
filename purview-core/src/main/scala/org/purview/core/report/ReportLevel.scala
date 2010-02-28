package org.purview.core.report

import org.purview.core.data.Color
import org.purview.core.data.Colors

case class ReportLevel(name: String) extends NotNull

trait LevelColor extends ReportLevel {
  def color: Color
}

object Debug extends ReportLevel("Debug")
                with LevelColor {
  val color = Color(1, 0, 0.5f, 0)
}

object Information extends ReportLevel("Information")
                      with LevelColor  {
  val color = Colors.Green
}

object Warning extends ReportLevel("Warning")
                  with LevelColor {
  val color = Colors.Yellow
}

object Error extends ReportLevel("Error")
                with LevelColor {
  val color = Colors.Red
}

object Critical extends ReportLevel("Critical")
                   with LevelColor {
  val color = Colors.Orange
}
