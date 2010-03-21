package org.purview.core.report

import java.awt.geom.Ellipse2D
import java.awt.geom.Rectangle2D
import java.awt.image.BufferedImage

abstract class ReportEntry extends NotNull {
  val level: ReportLevel
  override def toString = "Report entry"
}

trait Message extends ReportEntry {
  val message: String
  override def toString = message
}

trait Image extends ReportEntry with Point {
  val image: BufferedImage
}

trait Point extends ReportEntry {
  val x: Int
  val y: Int
}

trait FreeShape extends ReportEntry {
  def shape: java.awt.Shape
}

trait Circle extends FreeShape with Point {
  val radius: Int
  def shape = new Ellipse2D.Float(x - radius, y - radius, radius * 2, radius * 2)
}

trait Rectangle extends FreeShape with Point {
  val width: Int
  val height: Int
  def shape = new Rectangle2D.Float(x, y, width, height)
}

trait SourcePoint extends ReportEntry with Point {
  val sourceX: Int
  val sourceY: Int
}

trait FreeSourceShape extends ReportEntry {
  def sourceShape: java.awt.Shape
}

trait SourceCircle extends FreeSourceShape with SourcePoint {
  val sourceRadius: Int
  def sourceShape = new Ellipse2D.Float(sourceX - sourceRadius, sourceY - sourceRadius, sourceRadius * 2, sourceRadius * 2)
}

trait SourceRectangle extends FreeSourceShape with SourcePoint {
  val sourceWidth: Int
  val sourceHeight: Int
  def sourceShape = new Rectangle2D.Float(sourceX, sourceY, sourceWidth, sourceHeight)
}

case class ReportRectangle(level: ReportLevel, message: String, x: Int, y: Int, width: Int, height: Int)
    extends ReportEntry with Message with Rectangle

case class ReportCircle(level: ReportLevel, message: String, x: Int, y: Int, radius: Int)
    extends ReportEntry with Message with Circle

case class ReportRectangleMove(level: ReportLevel, message: String, sourceX: Int, sourceY: Int, x: Int, y: Int, width: Int, height: Int)
    extends ReportEntry with Message with Rectangle with SourceRectangle {
  val sourceWidth = width
  val sourceHeight = height
}

case class ReportCircleMove(level: ReportLevel, message: String, sourceX: Int, sourceY: Int, x: Int, y: Int, radius: Int)
    extends ReportEntry with Message with Circle with SourceCircle {
  val sourceRadius = radius
}
