package org.purview.core.report

import java.io.Serializable
import org.purview.core.data.Color
import org.purview.core.data.Matrix
import org.purview.core.data.plot.PlotEntry
import org.purview.core.data.shape.ShapeCommand

sealed abstract class ReportEntry extends NotNull with Serializable {
  val level: ReportLevel
  val message: String
  override def toString = "Report entry"
}

sealed case class ReportMessage(level: ReportLevel, message: String) extends ReportEntry

sealed case class ReportImage(level: ReportLevel, message: String,
                              x: Float, y: Float, image: Matrix[Color]) extends ReportEntry

sealed case class ReportRectangle(level: ReportLevel, message: String,
                                  x: Float, y: Float,
                                  width: Float, height: Float) extends ReportEntry

sealed case class ReportCircle(level: ReportLevel, message: String,
                               x: Float, y: Float, radius: Float) extends ReportEntry

sealed case class ReportRectangleMove(level: ReportLevel, message: String,
                                      sourceX: Float, sourceY: Float,
                                      x: Float, y: Float,
                                      width: Float, height: Float) extends ReportEntry

sealed case class ReportCircleMove(level: ReportLevel, message: String,
                                   sourceX: Float, sourceY: Float,
                                   x: Float, y: Float, radius: Float) extends ReportEntry

sealed case class ReportShape(level: ReportLevel, message: String,
                              shapeEntries: Seq[ShapeCommand]) extends ReportEntry

sealed case class ReportShapeMove(level: ReportLevel, message: String,
                                  sourceShapeEntries: Seq[ShapeCommand],
                                  shapeEntries: Seq[ShapeCommand]) extends ReportEntry

sealed case class ReportPlot(level: ReportLevel, message: String, plotEntries: Seq[PlotEntry]) extends ReportEntry
