package org.purview.core.report

abstract class ReportEntry extends NotNull {
  val level: ReportLevel
  override def toString = "Report entry"
}

trait Message extends ReportEntry {
  val message: String
  override def toString = message
}

trait Point extends ReportEntry {
  val x: Int
  val y: Int
}

trait Circle extends ReportEntry { this: Point =>
  val radius: Int
}

trait Rectangle extends ReportEntry { this: Point =>
  val width: Int
  val height: Int
}

trait SourcePoint extends ReportEntry { this: Point =>
  val sourceX: Int
  val sourceY: Int
}

trait SourceCircle extends ReportEntry { this: SourcePoint =>
  val sourceRadius: Int
}

trait SourceRectangle extends ReportEntry { this: SourcePoint =>
  val sourceWidth: Int
  val sourceHeight: Int
}

case class ReportRectangle(level: ReportLevel, message: String, x: Int, y: Int, width: Int, height: Int)
    extends ReportEntry with Message with Point with Rectangle
  
case class ReportCircle(level: ReportLevel, message: String, x: Int, y: Int, radius: Int)
    extends ReportEntry with Message with Point with Circle

case class ReportRectangleMove(level: ReportLevel, message: String, sourceX: Int, sourceY: Int, x: Int, y: Int, width: Int, height: Int)
    extends ReportEntry with Message with Point with Rectangle with SourcePoint with SourceRectangle {
  val sourceWidth = width
  val sourceHeight = height
}

case class ReportCircleMove(level: ReportLevel, message: String, sourceX: Int, sourceY: Int, x: Int, y: Int, radius: Int)
    extends ReportEntry with Message with Point with Circle with SourcePoint with SourceCircle {
  val sourceRadius = radius
}
