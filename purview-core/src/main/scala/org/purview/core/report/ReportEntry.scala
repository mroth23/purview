package org.purview.core.report

import java.io.Serializable
import org.purview.core.data.Color
import org.purview.core.data.Matrix

sealed abstract class ReportEntry extends NotNull with Serializable {
  val level: ReportLevel
  val message: String
  override def toString = "Report entry"
}

sealed case class ReportMessage(level: ReportLevel, message: String) extends ReportEntry

sealed case class ReportImage(level: ReportLevel, message: String,
                              x: Int, y: Int, image: Matrix[Color]) extends ReportEntry

sealed case class ReportRectangle(level: ReportLevel, message: String,
                                  x: Int, y: Int,
                                  width: Int, height: Int) extends ReportEntry

sealed case class ReportCircle(level: ReportLevel, message: String,
                               x: Int, y: Int, radius: Int) extends ReportEntry

sealed case class ReportRectangleMove(level: ReportLevel, message: String,
                                      sourceX: Int, sourceY: Int,
                                      x: Int, y: Int,
                                      width: Int, height: Int) extends ReportEntry

sealed case class ReportCircleMove(level: ReportLevel, message: String,
                                   sourceX: Int, sourceY: Int,
                                   x: Int, y: Int, radius: Int) extends ReportEntry

package shape {
  sealed abstract class ReportShapeCommand

  sealed case class ShapeMoveTo(x: Float, y: Float) extends ReportShapeCommand
  sealed case class ShapeLineTo(x: Float, y: Float) extends ReportShapeCommand
  sealed case class ShapeQuadTo(x0: Float, y0: Float, x1: Float, y1: Float) extends ReportShapeCommand
  sealed case class ShapeCubicTo(x0: Float, y0: Float, x1: Float, y1: Float, x2: Float, y2: Float) extends ReportShapeCommand
  case object ShapeClose extends ReportShapeCommand

  case object ShapeUseOddEvenFill extends ReportShapeCommand
  case object ShapeUseWindingFill extends ReportShapeCommand
}

sealed case class ReportShape(level: ReportLevel, message: String,
                              shapeEntries: Seq[shape.ReportShapeCommand]) extends ReportEntry

sealed case class ReportShapeMove(level: ReportLevel, message: String,
                                  sourceShapeEntries: Seq[shape.ReportShapeCommand],
                                  shapeEntries: Seq[shape.ReportShapeCommand]) extends ReportEntry

package plot {
  sealed abstract class ReportPlotEntry

  sealed case class ReportPlotPoint(x: Float, y: Float, z: Float, color: Color) extends ReportPlotEntry
  sealed case class ReportPlotVector(xDir: Float, yDir: Float, zDir: Float, color: Color) extends ReportPlotEntry
}

sealed case class ReportPlot(level: ReportLevel, message: String, plotEntries: Seq[plot.ReportPlotEntry]) extends ReportEntry
