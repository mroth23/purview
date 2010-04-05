package org.purview.qtui

import com.trolltech.qt.core.Qt
import com.trolltech.qt.gui.QBrush
import com.trolltech.qt.gui.QColor
import com.trolltech.qt.gui.QGraphicsItem
import com.trolltech.qt.gui.QGraphicsScene
import com.trolltech.qt.gui.QGraphicsView
import com.trolltech.qt.gui.QImage
import com.trolltech.qt.gui.QPalette
import com.trolltech.qt.gui.QPixmap
import com.trolltech.qt.opengl.QGLWidget
import java.awt.image.BufferedImage
import org.purview.core.report.ReportEntry
import org.purview.qtui.meta.ImageSession

case class ImageSessionWidget(imageSession: ImageSession) extends QGraphicsView {
  setWindowTitle(imageSession.imageFile.getName)
  setBackgroundRole(QPalette.ColorRole.Dark)
  setViewport(new QGLWidget) //OpenGL drawing acceleration
  setDragMode(QGraphicsView.DragMode.ScrollHandDrag)
  setInteractive(true)

  private val bgBrush = new QBrush(palette.color(QPalette.ColorRole.Mid), Qt.BrushStyle.Dense4Pattern) {
    setTransform(transform.scale(16, 16))
  }
  
  setBackgroundBrush(bgBrush)

  private val pixmap = { //Convert the image to a QPixmap for performance's sake
    val tmp = new QPixmap(imageSession.imageFile.getAbsolutePath)
    if(tmp.isNull)
      bufferedImageToPixmap(imageSession.matrix.image)
    else
      tmp
  }

  private val sessionScene = new QGraphicsScene
  sessionScene.addPixmap(pixmap)
  setScene(sessionScene)

  private var graphicsItem: Option[QGraphicsItem] = None

  var _currentReportEntry: Option[ReportEntry] = None
  def currentReportEntry = _currentReportEntry
  def currentReportEntry_=(reportEntry: Option[ReportEntry]) = {
    _currentReportEntry = reportEntry
    graphicsItem.foreach(sessionScene.removeItem)
    println("TODO: draw report entry " + reportEntry) //TODO: draw report entry
  }

  def configureAnalysers() {
    println("TODO: configure analysers") //TODO: configure analysers
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
