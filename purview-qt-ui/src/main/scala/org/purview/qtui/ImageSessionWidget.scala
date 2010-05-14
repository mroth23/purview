package org.purview.qtui

import com.trolltech.qt.core.QPointF
import com.trolltech.qt.core.Qt
import com.trolltech.qt.gui.QBrush
import com.trolltech.qt.gui.QColor
import com.trolltech.qt.gui.QGraphicsEllipseItem
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
import org.purview.core.data.Color
import org.purview.core.data.Matrix
import org.purview.core.report.LevelColor
import org.purview.core.report.ReportCircle
import org.purview.core.report.ReportCircleMove
import org.purview.core.report.ReportEntry
import org.purview.core.report.ReportImage
import org.purview.core.report.ReportRectangle
import org.purview.core.report.ReportRectangleMove
import org.purview.core.report.ReportShape
import org.purview.core.report.ReportShapeMove
import org.purview.core.data.shape._
import org.purview.qtui.meta.ImageSession
import scala.collection.mutable.WeakHashMap
import scala.math._

case class ImageSessionWidget(imageSession: ImageSession) extends QGraphicsView {
  setWindowTitle(imageSession.imageFile.getName)
  setBackgroundRole(QPalette.ColorRole.Dark)
  setDragMode(QGraphicsView.DragMode.ScrollHandDrag)
  //setViewport(new QGLWidget) //Use OpenGL for drawing
  setRenderHint(QPainter.RenderHint.Antialiasing)
  setRenderHint(QPainter.RenderHint.HighQualityAntialiasing)
  setInteractive(true)

  private val bgBrush = new QBrush(palette.color(QPalette.ColorRole.Mid), Qt.BrushStyle.Dense4Pattern)
  bgBrush.setTransform(transform.scale(16, 16))

  setBackgroundBrush(bgBrush)

  private val pixmap = { //Convert the image to a QPixmap for performance's sake
    val tmp = new QPixmap(imageSession.imageFile.getAbsolutePath)
    if(tmp.isNull)
      ImageUtils.convertToQPixmap(imageSession.matrix)
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

  def saveImageTo(file: String) = _currentReportEntry match {
    case Some(entry) =>
      import com.trolltech.qt.svg.QSvgGenerator
      val generator = new QSvgGenerator
      generator.setFileName(file)
      generator.setSize(sceneRect.size.toSize)
      generator.setViewBox(sceneRect)
      generator.setTitle("Purview - " + imageSession.imageFile.getName)
      generator.setDescription("Generated from " + entry.message)

      import com.trolltech.qt.gui.QPainter
      val painter = new QPainter
      painter.begin(generator)
      sessionScene.render(painter)
      painter.end()
    case None => //TODO: notify user
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
  private val pixmapCache = new WeakHashMap[Matrix[Color], QPixmap]
  private val painterPathCache = new WeakHashMap[Seq[ShapeCommand], QPainterPath]

  def convertToQPixmap(image: Matrix[Color]) = pixmapCache.getOrElseUpdate(image, matrixToPixmap(image))

  def convertToQPainterPath(shape: Seq[ShapeCommand]) = painterPathCache.getOrElseUpdate(shape, shapeEntriesToPainterPath(shape))

  val ArrowWidth = 12f
  val ArrowAngle = toRadians(20)

  def makeArrow(p1: QPointF, p2: QPointF): QPainterPath = {
    val p = new QPainterPath
    val dx = p2.x - p1.x
    val dy = p2.y - p1.y
    val theta = atan2(dy, dx) + Pi

    val mx = (p2.x + ArrowWidth / 2 * cos(theta)).toFloat
    val my = (p2.y + ArrowWidth / 2 * sin(theta)).toFloat
    val ax1 = p2.x + ArrowWidth * cos(theta + ArrowAngle)
    val ay1 = p2.y + ArrowWidth * sin(theta + ArrowAngle)
    val ax2 = p2.x + ArrowWidth * cos(theta - ArrowAngle)
    val ay2 = p2.y + ArrowWidth * sin(theta - ArrowAngle)

    p.moveTo(p2.x, p2.y)
    p.lineTo(ax1, ay1)
    p.lineTo(ax2, ay2)
    p.closeSubpath()

    p.moveTo(p1.x, p1.y)
    p.lineTo(mx, my)
    p
  }

  /** Convert a seq of shape commands to a Qt PainterPath */
  private def shapeEntriesToPainterPath(shape: Seq[ShapeCommand]) = {
    val path = new QPainterPath
    shape.foreach({
        case ShapeUseOddEvenFill =>
          path.setFillRule(Qt.FillRule.OddEvenFill)
        case ShapeUseWindingFill =>
          path.setFillRule(Qt.FillRule.WindingFill)
        case ShapeMoveTo(x, y) =>
          path.moveTo(x, y)
        case ShapeLineTo(x, y) =>
          path.lineTo(x, y)
        case ShapeQuadTo(x0, y0, x1, y1) =>
          path.quadTo(x0, y0, x1, y1)
        case ShapeCubicTo(x0, y0, x1, y1, x2, y2) =>
          path.cubicTo(x0, y0, x1, y1, x2, y2)
        case ShapeClose =>
          path.closeSubpath()
      })
    path
  }

  /** Convert a color matrix to a Qt Image */
  private def matrixToPixmap(matrix: Matrix[Color]) = {
    val result = new QImage(matrix.width, matrix.height, QImage.Format.Format_ARGB32)
    val w = matrix.width
    val h = matrix.height
    var x = 0
    while(x < w) {
      var y = 0
      while(y < h) {
        result.setPixel(x, y, matrix(x, y).toRGB)
        y += 1
      }
      x += 1
    }
    QPixmap.fromImage(result)
  }
}
