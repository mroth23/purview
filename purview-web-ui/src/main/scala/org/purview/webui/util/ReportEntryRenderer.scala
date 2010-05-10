package org.purview.webui.util

import java.awt.BasicStroke
import java.awt.Color
import java.awt.Graphics2D
import java.awt.Shape
import java.awt.geom.GeneralPath
import java.awt.geom.Ellipse2D
import java.awt.geom.Line2D
import java.awt.geom.Path2D
import java.awt.geom.Rectangle2D
import java.awt.geom.PathIterator
import java.awt.image.BufferedImage
import org.purview.core.data.Matrix
import org.purview.core.data.{Color => PurviewColor}
import org.purview.core.report.LevelColor
import org.purview.core.report.ReportCircle
import org.purview.core.report.ReportCircleMove
import org.purview.core.report.ReportEntry
import org.purview.core.report.ReportImage
import org.purview.core.report.ReportRectangle
import org.purview.core.report.ReportRectangleMove
import org.purview.core.report.ReportShape
import org.purview.core.report.ReportShapeMove
import org.purview.core.report.shape._
import scala.math._

object ReportEntryRenderer {
  val PointRadius = 2f
  val StrokeWidth = 2f
  val ArrowWidth = 12f
  val ArrowAngle = toRadians(20)
  val sourceColor = Color.blue
  val transpSourceColor =
    new Color(sourceColor.getRed, sourceColor.getGreen,
              sourceColor.getBlue, sourceColor.getAlpha / 2)

  val stroke = new BasicStroke(StrokeWidth, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND)
  val squareStroke = new BasicStroke(StrokeWidth, BasicStroke.CAP_SQUARE, BasicStroke.JOIN_MITER)

  def renderReportEntry(g: Graphics2D, entry: ReportEntry) = {
    val level = entry.level

    val color = level match {
      case c: LevelColor => c.color.toAWTColor
      case _ => Color.red
    }
    val transpColor = new Color(color.getRed, color.getGreen, color.getBlue, color.getAlpha / 2)

    g.setStroke(stroke)

    entry match {
        case ReportImage(_, _, x, y, matrix) =>
          g.drawImage(matrixToBufferedImage(matrix), x.toInt, y.toInt, null)
        case ReportRectangle(_, _, x, y, width, height) =>
          makeShape(g, new Rectangle2D.Float(x, y, width, height), transpColor, color)
        case ReportCircle(_, _, x, y, radius) =>
          makeShape(g, new Ellipse2D.Float(x - radius, y - radius, radius * 2, radius * 2), transpColor, color)
        case ReportShape(_, _, shape) =>
          makeShape(g, shapeCommandsToGeneralPath(shape), transpColor, color)
        case ReportRectangleMove(_, _, srcX, srcY, tgtX, tgtY, width, height) =>
          makeShape(g, new Rectangle2D.Float(srcX, srcY, width, height), transpSourceColor, sourceColor)
          makeShape(g, new Rectangle2D.Float(tgtX, tgtY, width, height), transpColor, color)
          makeArrow(g, srcX, srcY, tgtX, tgtY, Color.white)
        case ReportCircleMove(_, _, srcX, srcY, tgtX, tgtY, radius) =>
          makeShape(g, new Rectangle2D.Float(srcX - radius, srcY - radius, radius * 2, radius * 2), transpSourceColor, sourceColor)
          makeShape(g, new Rectangle2D.Float(tgtX - radius, tgtY - radius, radius * 2, radius * 2), transpColor, color)
          makeArrow(g, srcX, srcY, tgtX, tgtY, Color.white)
        case ReportShapeMove(_, _, sourceShape, targetShape) =>
          val source = shapeCommandsToGeneralPath(sourceShape)
          val target = shapeCommandsToGeneralPath(targetShape)
          makeShape(g, source, transpSourceColor, sourceColor)
          makeShape(g, target, transpColor, color)
          makeArrow(g,
                    source.getBounds.getCenterX.toFloat, source.getBounds.getCenterY.toFloat,
                    target.getBounds.getCenterX.toFloat, target.getBounds.getCenterY.toFloat,
                    Color.white)
        case _ =>
          None
      }
  }

  def makePoint(g: Graphics2D, x: Float, y: Float, fill: Color, outline: Color) {
    val point = new Ellipse2D.Float(x - PointRadius, y - PointRadius, PointRadius * 2, PointRadius * 2)
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
    val theta = atan2(dy, dx) + Pi

    val mx = x2 + ArrowWidth / 2 * cos(theta)
    val my = y2 + ArrowWidth / 2 * sin(theta)
    val ax1 = x2 + ArrowWidth * cos(theta + ArrowAngle)
    val ay1 = y2 + ArrowWidth * sin(theta + ArrowAngle)
    val ax2 = x2 + ArrowWidth * cos(theta - ArrowAngle)
    val ay2 = y2 + ArrowWidth * sin(theta - ArrowAngle)

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

  private def matrixToBufferedImage(matrix: Matrix[PurviewColor]) = {
    val result = new BufferedImage(matrix.width, matrix.height, BufferedImage.TYPE_INT_ARGB)
    val w = matrix.width
    val h = matrix.height
    var x = 0
    while(x < w) {
      var y = 0
      while(y < h) {
        result.setRGB(x, y, matrix(x, y).toRGB)
        y += 1
      }
      x += 1
    }
    result
  }

  private def shapeCommandsToGeneralPath(shape: Seq[ShapeCommand]) = {
    val path = new GeneralPath(PathIterator.WIND_NON_ZERO, shape.length)
    shape.foreach({
        case ShapeUseOddEvenFill =>
          path.setWindingRule(PathIterator.WIND_EVEN_ODD)
        case ShapeUseWindingFill =>
          path.setWindingRule(PathIterator.WIND_NON_ZERO)
        case ShapeMoveTo(x, y) =>
          path.moveTo(x, y)
        case ShapeLineTo(x, y) =>
          path.lineTo(x, y)
        case ShapeQuadTo(x0, y0, x1, y1) =>
          path.quadTo(x0, y0, x1, y1)
        case ShapeCubicTo(x0, y0, x1, y1, x2, y2) =>
          path.curveTo(x0, y0, x1, y1, x2, y2)
        case ShapeClose =>
          path.closePath()
      })
    path
  }
}
