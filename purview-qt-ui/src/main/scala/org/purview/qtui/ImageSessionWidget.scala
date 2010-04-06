package org.purview.qtui

import com.trolltech.qt.core.QPointF
import com.trolltech.qt.core.QTimeLine
import com.trolltech.qt.core.Qt
import com.trolltech.qt.gui.QBrush
import com.trolltech.qt.gui.QColor
import com.trolltech.qt.gui.QGraphicsEllipseItem
import com.trolltech.qt.gui.QGraphicsItem
import com.trolltech.qt.gui.QGraphicsItemAnimation
import com.trolltech.qt.gui.QGraphicsItemGroup
import com.trolltech.qt.gui.QGraphicsItemInterface
import com.trolltech.qt.gui.QGraphicsPathItem
import com.trolltech.qt.gui.QGraphicsPixmapItem
import com.trolltech.qt.gui.QGraphicsRectItem
import com.trolltech.qt.gui.QGraphicsScene
import com.trolltech.qt.gui.QGraphicsView
import com.trolltech.qt.gui.QImage
import com.trolltech.qt.gui.QPainter
import com.trolltech.qt.gui.QPainterPath
import com.trolltech.qt.gui.QPalette
import com.trolltech.qt.gui.QPen
import com.trolltech.qt.gui.QPixmap
import com.trolltech.qt.opengl.QGLWidget
import java.awt.Shape
import java.awt.geom.PathIterator
import java.awt.image.BufferedImage
import org.purview.core.report.LevelColor
import org.purview.core.report.ReportCircle
import org.purview.core.report.ReportCircleMove
import org.purview.core.report.ReportEntry
import org.purview.core.report.ReportImage
import org.purview.core.report.ReportRectangle
import org.purview.core.report.ReportRectangleMove
import org.purview.core.report.ReportShape
import org.purview.core.report.ReportShapeMove
import org.purview.qtui.meta.ImageSession
import scala.collection.mutable.WeakHashMap

case class ImageSessionWidget(imageSession: ImageSession) extends QGraphicsView {
  setWindowTitle(imageSession.imageFile.getName)
  setBackgroundRole(QPalette.ColorRole.Dark)
  setDragMode(QGraphicsView.DragMode.ScrollHandDrag)
  setViewport(new QGLWidget) //Use OpenGL for drawing
  setRenderHint(QPainter.RenderHint.Antialiasing)
  setRenderHint(QPainter.RenderHint.HighQualityAntialiasing)
  setInteractive(true)

  private val bgBrush = new QBrush(palette.color(QPalette.ColorRole.Mid), Qt.BrushStyle.Dense4Pattern)
  bgBrush.setTransform(transform.scale(16, 16))

  setBackgroundBrush(bgBrush)

  private val pixmap = { //Convert the image to a QPixmap for performance's sake
    val tmp = new QPixmap(imageSession.imageFile.getAbsolutePath)
    if(tmp.isNull)
      ImageUtils.convertToQPixmap(imageSession.matrix.image)
    else
      tmp
  }

  private val sessionScene = new QGraphicsScene
  sessionScene.addPixmap(pixmap)
  setScene(sessionScene)

  private var graphicsItem: Option[QGraphicsItemInterface] = None

  def analyse() = {
    imageSession.analyse()
    imageSession.analysis.foreach(_.reportEntryChanged ::= currentReportEntry_=)
  }

  val srcColor = QColor.blue
  val transpSrcColor = srcColor.clone
  transpSrcColor.setAlphaF(srcColor.alphaF / 2)
  val srcBrush = new QBrush(transpSrcColor)
  val srcPen = new QPen(srcColor, 2)
  srcPen.setCapStyle(Qt.PenCapStyle.SquareCap)
  srcPen.setJoinStyle(Qt.PenJoinStyle.RoundJoin)

  val arrowPen = new QPen(QColor.white, 2)
  arrowPen.setJoinStyle(Qt.PenJoinStyle.MiterJoin)
  val arrowBrush = new QBrush(QColor.white)

  private var _currentReportEntry: Option[ReportEntry] = None
  def currentReportEntry = _currentReportEntry
  def currentReportEntry_=(reportEntry: Option[ReportEntry]) = {
    _currentReportEntry = reportEntry
    graphicsItem.foreach(sessionScene.removeItem)

    for(entry <- reportEntry) {
      val color = entry.level match {
        case l: LevelColor =>
          QColor.fromRgb(l.color.toRGB)
        case _ =>
          QColor.red
      }
      val transpColor = color.clone
      transpColor.setAlphaF(color.alphaF / 2)
      val brush = new QBrush(transpColor)
      val pen = new QPen(color, 2)
      pen.setCapStyle(Qt.PenCapStyle.SquareCap)
      pen.setJoinStyle(Qt.PenJoinStyle.RoundJoin)

      graphicsItem = entry match {
        case ReportImage(_, _, x, y, image) =>
          val pixmap = new QGraphicsPixmapItem(ImageUtils.convertToQPixmap(image))
          pixmap.setOffset(x, y)
          Some(pixmap)
        case ReportRectangle(_, _, x, y, width, height) =>
          val rect = new QGraphicsRectItem(x, y, width, height)
          rect.setPen(pen)
          rect.setBrush(brush)
          Some(rect)
        case ReportCircle(_, _, x, y, radius) =>
          val ellipse = new QGraphicsEllipseItem(x - radius, y - radius, radius * 2, radius * 2)
          ellipse.setPen(pen)
          ellipse.setBrush(brush)
          Some(ellipse)
        case ReportShape(_, _, shape) =>
          val path = new QGraphicsPathItem(ImageUtils.convertToQPainterPath(shape))
          path.setPen(pen)
          path.setBrush(brush)
          Some(path)
        case ReportRectangleMove(_, _, srcX, srcY, tgtX, tgtY, width, height) =>
          val source = new QGraphicsRectItem(srcX, srcY, width, height)
          source.setPen(srcPen)
          source.setBrush(srcBrush)
          val target = new QGraphicsRectItem(tgtX, tgtY, width, height)
          target.setPen(pen)
          target.setBrush(brush)
          val arrow = new QGraphicsPathItem(ImageUtils.makeArrow(source.boundingRect.center, target.boundingRect.center))
          arrow.setPen(arrowPen)
          arrow.setBrush(arrowBrush)
          val group = new QGraphicsItemGroup
          group.addToGroup(source)
          group.addToGroup(target)
          group.addToGroup(arrow)
          Some(group)
        case ReportCircleMove(_, _, srcX, srcY, tgtX, tgtY, radius) =>
          val source = new QGraphicsEllipseItem(srcX - radius, srcY - radius, radius * 2, radius * 2)
          source.setPen(srcPen)
          source.setBrush(srcBrush)
          val target = new QGraphicsEllipseItem(tgtX - radius, tgtY - radius, radius * 2, radius * 2)
          target.setPen(pen)
          target.setBrush(brush)
          val arrow = new QGraphicsPathItem(ImageUtils.makeArrow(source.boundingRect.center, target.boundingRect.center))
          arrow.setPen(arrowPen)
          arrow.setBrush(arrowBrush)
          val group = new QGraphicsItemGroup
          group.addToGroup(source)
          group.addToGroup(target)
          group.addToGroup(arrow)
          Some(group)
        case ReportShapeMove(_, _, sourceShape, targetShape) =>
          val source = new QGraphicsPathItem(ImageUtils.convertToQPainterPath(sourceShape))
          source.setPen(srcPen)
          source.setBrush(srcBrush)
          val target = new QGraphicsPathItem(ImageUtils.convertToQPainterPath(targetShape))
          target.setPen(pen)
          target.setBrush(brush)
          val arrow = new QGraphicsPathItem(ImageUtils.makeArrow(source.boundingRect.center, target.boundingRect.center))
          arrow.setPen(arrowPen)
          arrow.setBrush(arrowBrush)
          val group = new QGraphicsItemGroup
          group.addToGroup(source)
          group.addToGroup(target)
          group.addToGroup(arrow)
          Some(group)
        case _ =>
          None
      }

      graphicsItem.foreach { item =>
        sessionScene.addItem(item)
        ImageSessionWidget.this.ensureVisible(item, 16, 16)
      }
    }
  }

  def configureAnalysers() {
    val diag = new SettingsDialog(imageSession, this)
    diag.exec()
  }
}

object ImageUtils {
  private val pixmapCache = new WeakHashMap[BufferedImage, QPixmap]
  private val painterPathCache = new WeakHashMap[Shape, QPainterPath]

  def convertToQPixmap(image: BufferedImage) = pixmapCache.getOrElseUpdate(image, bufferedImageToPixmap(image))

  def convertToQPainterPath(shape: Shape) = painterPathCache.getOrElseUpdate(shape, shapeToPainterPath(shape))

  val ArrowWidth = 12f
  val ArrowAngle = math.toRadians(20)

  def makeArrow(p1: QPointF, p2: QPointF): QPainterPath = {
    val p = new QPainterPath
    val dx = p2.x - p1.x
    val dy = p2.y - p1.y
    val theta = math.atan2(dy, dx) + math.Pi

    val mx = (p2.x + ArrowWidth / 2 * math.cos(theta)).toFloat
    val my = (p2.y + ArrowWidth / 2 * math.sin(theta)).toFloat
    val ax1 = p2.x + ArrowWidth * math.cos(theta + ArrowAngle)
    val ay1 = p2.y + ArrowWidth * math.sin(theta + ArrowAngle)
    val ax2 = p2.x + ArrowWidth * math.cos(theta - ArrowAngle)
    val ay2 = p2.y + ArrowWidth * math.sin(theta - ArrowAngle)

    p.moveTo(p2.x, p2.y)
    p.lineTo(ax1, ay1)
    p.lineTo(ax2, ay2)
    p.closeSubpath()

    p.moveTo(p1.x, p1.y)
    p.lineTo(mx, my)
    p
  }

  private def shapeToPainterPath(shape: Shape) = {
    val path = new QPainterPath
    val iter = shape.getPathIterator(null)
    val buffer = new Array[Float](6)
    var prevWind = -1
    while(!iter.isDone) {
      iter.getWindingRule match {
        case PathIterator.WIND_EVEN_ODD if prevWind != PathIterator.WIND_EVEN_ODD =>
          path.setFillRule(Qt.FillRule.OddEvenFill)
          prevWind = PathIterator.WIND_EVEN_ODD
        case PathIterator.WIND_NON_ZERO if prevWind != PathIterator.WIND_NON_ZERO =>
          path.setFillRule(Qt.FillRule.WindingFill)
          prevWind = PathIterator.WIND_NON_ZERO
        case _ => //nothing
      }
      iter.currentSegment(buffer) match {
        case PathIterator.SEG_MOVETO =>
          path.moveTo(buffer(0), buffer(1))
        case PathIterator.SEG_LINETO =>
          path.lineTo(buffer(0), buffer(1))
        case PathIterator.SEG_QUADTO =>
          path.quadTo(buffer(0), buffer(1), buffer(2), buffer(3))
        case PathIterator.SEG_CUBICTO =>
          path.cubicTo(buffer(0), buffer(1), buffer(2), buffer(3), buffer(4), buffer(5))
        case PathIterator.SEG_CLOSE =>
          path.closeSubpath()
      }
      iter.next()
    }
    path
  }

  private def bufferedImageToPixmap(bufferedImage: BufferedImage) = {
    val result = new QImage(bufferedImage.getWidth, bufferedImage.getHeight, QImage.Format.Format_ARGB32)
    val w = bufferedImage.getWidth
    val h = bufferedImage.getHeight
    var x = 0
    while(x < w) {
      var y = 0
      while(y < h) {
        result.setPixel(x, y, bufferedImage.getRGB(x, y))
        y += 1
      }
      x += 1
    }
    QPixmap.fromImage(result)
  }
}
