package org.purview.webui.util

import java.awt.BasicStroke
import java.awt.Color
import java.awt.Graphics2D
import java.awt.Shape
import java.awt.geom.Ellipse2D
import java.awt.geom.Line2D
import java.awt.geom.Path2D
import java.awt.geom.Rectangle2D
import org.purview.core.report.LevelColor
import org.purview.core.report.ReportCircle
import org.purview.core.report.ReportCircleMove
import org.purview.core.report.ReportEntry
import org.purview.core.report.ReportImage
import org.purview.core.report.ReportRectangle
import org.purview.core.report.ReportRectangleMove
import org.purview.core.report.ReportShape
import org.purview.core.report.ReportShapeMove

object ReportEntryRenderer {
  val POINT_RADIUS = 2f
  val STROKE_WIDTH = 2f
  val ARROW_WIDTH = 12f
  val ARROW_ANGLE = Math.toRadians(20)
  val sourceColor = Color.blue
  val transpSourceColor =
    new Color(sourceColor.getRed, sourceColor.getGreen,
              sourceColor.getBlue, sourceColor.getAlpha / 2)

  val stroke = new BasicStroke(STROKE_WIDTH, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND)
  val squareStroke = new BasicStroke(STROKE_WIDTH, BasicStroke.CAP_SQUARE, BasicStroke.JOIN_MITER)

  def renderReportEntry(g: Graphics2D, entry: ReportEntry) = {
    val level = entry.level

    val color = level match {
      case c: LevelColor => c.color.toAWTColor
      case _ => Color.red
    }
    val transpColor = new Color(color.getRed, color.getGreen, color.getBlue, color.getAlpha / 2)

    g.setStroke(stroke)

    entry match {
        case ReportImage(_, _, x, y, image) =>
          g.drawImage(image, x, y, null)
        case ReportRectangle(_, _, x, y, width, height) =>
          makeShape(g, new Rectangle2D.Float(x, y, width, height), transpColor, color)
        case ReportCircle(_, _, x, y, radius) =>
          makeShape(g, new Ellipse2D.Float(x - radius, y - radius, radius * 2, radius * 2), transpColor, color)
        case ReportShape(_, _, shape) =>
          makeShape(g, shape, transpColor, color)
        case ReportRectangleMove(_, _, srcX, srcY, tgtX, tgtY, width, height) =>
          makeShape(g, new Rectangle2D.Float(srcX, srcY, width, height), transpSourceColor, sourceColor)
          makeShape(g, new Rectangle2D.Float(tgtX, tgtY, width, height), transpColor, color)
          makeArrow(g, srcX, srcY, tgtX, tgtY, Color.white)
        case ReportCircleMove(_, _, srcX, srcY, tgtX, tgtY, radius) =>
          makeShape(g, new Rectangle2D.Float(srcX - radius, srcY - radius, radius * 2, radius * 2), transpSourceColor, sourceColor)
          makeShape(g, new Rectangle2D.Float(tgtX - radius, tgtY - radius, radius * 2, radius * 2), transpColor, color)
          makeArrow(g, srcX, srcY, tgtX, tgtY, Color.white)
        case ReportShapeMove(_, _, sourceShape, targetShape) =>
          makeShape(g, sourceShape, transpSourceColor, sourceColor)
          makeShape(g, targetShape, transpColor, color)
          makeArrow(g,
                    sourceShape.getBounds.getCenterX.toFloat, sourceShape.getBounds.getCenterY.toFloat,
                    targetShape.getBounds.getCenterX.toFloat, targetShape.getBounds.getCenterY.toFloat,
                    Color.white)
        case _ =>
          None
      }
  }

  def makePoint(g: Graphics2D, x: Float, y: Float, fill: Color, outline: Color) {
    val point = new Ellipse2D.Float(x - POINT_RADIUS, y - POINT_RADIUS, POINT_RADIUS * 2, POINT_RADIUS * 2)
    makeShape(g, point, fill, outline);
  }

  def makeShape(g: Graphics2D, shape: Shape, fill: Color, outline: Color) {
    //Fill for shape
    g.setPaint(fill)
    g.fill(shape)

    //Outline for shape
    g.setPaint(outline)
    g.draw(shape)
  }

  def makeArrow(g: Graphics2D, x1: Float, y1: Float, x2: Float, y2: Float, color: Color) {
    val path = new Path2D.Double

    val dx = x2 - x1
    val dy = y2 - y1
    val theta = Math.atan2(dy, dx) + Math.Pi

    val mx = x2 + ARROW_WIDTH / 2 * Math.cos(theta)
    val my = y2 + ARROW_WIDTH / 2 * Math.sin(theta)
    val ax1 = x2 + ARROW_WIDTH * Math.cos(theta + ARROW_ANGLE)
    val ay1 = y2 + ARROW_WIDTH * Math.sin(theta + ARROW_ANGLE)
    val ax2 = x2 + ARROW_WIDTH * Math.cos(theta - ARROW_ANGLE)
    val ay2 = y2 + ARROW_WIDTH * Math.sin(theta - ARROW_ANGLE)

    path.moveTo(x2, y2)
    path.lineTo(ax1, ay1)
    path.lineTo(ax2, ay2)
    path.closePath()

    val arrowLine = new Line2D.Double(x1, y1, mx, my)

    //Outline for arrow
    val prev = g.getStroke()
    g.setStroke(squareStroke)
    g.setPaint(color)
    g.draw(arrowLine)
    g.fill(path)
    g.setStroke(prev)
  }
}
