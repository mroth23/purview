package org.purview.core.report

abstract class ReportEntry extends NotNull {
  val level: ReportLevel
}

trait Message extends ReportEntry {
  val message: String
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
