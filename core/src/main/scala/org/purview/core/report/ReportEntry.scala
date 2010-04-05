package org.purview.core.report

import java.awt.Shape
import java.awt.geom.Ellipse2D
import java.awt.geom.Rectangle2D
import java.awt.image.BufferedImage
import java.io.Serializable

abstract class ReportEntry extends NotNull with Serializable {
  val level: ReportLevel
  override def toString = "Report entry"
}

trait Message {
  val message: String
}

trait FreeShape {
  def shape: Shape
}

sealed case class ReportImage(level: ReportLevel, message: String, x: Int, y: Int, image: BufferedImage)
    extends ReportEntry with Message

sealed case class ReportRectangle(level: ReportLevel, message: String, x: Int, y: Int, width: Int, height: Int)
    extends ReportEntry with FreeShape with Message {
  def shape = new Rectangle2D.Float(x, y, width, height)
}

sealed case class ReportCircle(level: ReportLevel, message: String, x: Int, y: Int, radius: Int)
    extends ReportEntry with Message with FreeShape {
  def shape = new Ellipse2D.Float(x - radius, y - radius, radius * 2, radius * 2)
}

sealed case class ReportShape(level: ReportLevel, message: String, shape: Shape)
    extends ReportEntry with FreeShape with Message

case class ReportRectangleMove(level: ReportLevel, message: String, sourceX: Int, sourceY: Int, x: Int, y: Int, width: Int, height: Int)
    extends ReportEntry with FreeShape with Message {
  def shape = new Rectangle2D.Float(x, y, width, height)
}

case class ReportCircleMove(level: ReportLevel, message: String, sourceX: Int, sourceY: Int, x: Int, y: Int, radius: Int)
    extends ReportEntry with FreeShape with Message {
  def shape = new Ellipse2D.Float(x - radius, y - radius, radius * 2, radius * 2)
}

case class ReportShapeMove(level: ReportLevel, message: String, sourceShape: Shape, shape: Shape)
    extends ReportEntry with FreeShape with Message
