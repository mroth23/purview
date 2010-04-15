package org.purview.core.report

import java.awt.Shape
import java.awt.geom.Ellipse2D
import java.awt.geom.Rectangle2D
import java.awt.image.BufferedImage
import java.io.Serializable
import org.purview.core.data.Color

sealed abstract class ReportEntry extends NotNull with Serializable {
  val level: ReportLevel
  val message: String
  override def toString = "Report entry"
}

trait FreeShape { this: ReportEntry =>
  def shape: Shape
}

sealed case class ReportMessage(level: ReportLevel, message: String) extends ReportEntry

sealed case class ReportImage(level: ReportLevel, message: String, x: Int, y: Int, image: BufferedImage)
    extends ReportEntry

sealed case class ReportRectangle(level: ReportLevel, message: String, x: Int, y: Int, width: Int, height: Int)
    extends ReportEntry with FreeShape {
  def shape = new Rectangle2D.Float(x, y, width, height)
}

sealed case class ReportCircle(level: ReportLevel, message: String, x: Int, y: Int, radius: Int)
    extends ReportEntry with FreeShape {
  def shape = new Ellipse2D.Float(x - radius, y - radius, radius * 2, radius * 2)
}

sealed case class ReportShape(level: ReportLevel, message: String, shape: Shape)
    extends ReportEntry with FreeShape

sealed case class ReportRectangleMove(level: ReportLevel, message: String, sourceX: Int, sourceY: Int, x: Int, y: Int, width: Int, height: Int)
    extends ReportEntry with FreeShape {
  def shape = new Rectangle2D.Float(x, y, width, height)
}

sealed case class ReportCircleMove(level: ReportLevel, message: String, sourceX: Int, sourceY: Int, x: Int, y: Int, radius: Int)
    extends ReportEntry with FreeShape {
  def shape = new Ellipse2D.Float(x - radius, y - radius, radius * 2, radius * 2)
}

sealed case class ReportShapeMove(level: ReportLevel, message: String, sourceShape: Shape, shape: Shape)
    extends ReportEntry with FreeShape

sealed abstract class ReportPlotEntry

sealed case class ReportPlotPoint(x: Float, y: Float, z: Float, color: Color) extends ReportPlotEntry

sealed case class ReportPlotVector(xDir: Float, yDir: Float, zDir: Float, color: Color) extends ReportPlotEntry

sealed case class ReportPlot(plotEntries: Seq[ReportPlotEntry]) extends ReportEntry
