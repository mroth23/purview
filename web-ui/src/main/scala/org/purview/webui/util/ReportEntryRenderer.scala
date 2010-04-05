package org.purview.webui.util

import java.awt.BasicStroke
import java.awt.Color
import java.awt.Graphics2D
import java.awt.Shape
import java.awt.geom.Ellipse2D
import java.awt.geom.Line2D
import java.awt.geom.Path2D
import org.purview.core.report.LevelColor
import org.purview.core.report.ReportEntry

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

    //TODO: fix drawing
    /*
    entry match {
      case point: Point =>
        if(!point.isInstanceOf[Image])
          makePoint(g, point.x, point.y, transpColor, color)

    }*/
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