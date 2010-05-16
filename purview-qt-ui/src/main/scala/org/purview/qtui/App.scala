package org.purview.qtui

import com.trolltech.qt.core.QCoreApplication
import com.trolltech.qt.core.QTimer
import com.trolltech.qt.core.Qt
import com.trolltech.qt.gui.QApplication
import com.trolltech.qt.gui.QPaintEvent
import com.trolltech.qt.gui.QPainter
import com.trolltech.qt.gui.QPixmap
import com.trolltech.qt.gui.QWidget
import com.trolltech.qt.opengl.QGLFormat

object App {
  private class SplashScreen(pixmap: QPixmap) extends QWidget {
    setWindowFlags(Qt.WindowType.FramelessWindowHint)
    setWindowFlags(Qt.WindowType.SplashScreen)
    setAttribute(Qt.WidgetAttribute.WA_TranslucentBackground)
    setAutoFillBackground(false)
    setWindowTitle("Loading Purview 1.2")
    resize(pixmap.width, pixmap.height)

    override def paintEvent(e: QPaintEvent) = {
      val g = new QPainter(this)
      g.setCompositionMode(QPainter.CompositionMode.CompositionMode_DestinationOver)
      g.drawPixmap(0, 0, pixmap)
    }
  }

  private lazy val pixmap = new QPixmap("classpath:pixmaps/splash.png")
  private lazy val splash = new SplashScreen(pixmap)

  def main(args: Array[String]) {
    QApplication.initialize(args)

    splash.show()
    QCoreApplication.processEvents()

    val format = QGLFormat.defaultFormat
    format.setSampleBuffers(true)
    QGLFormat.setDefaultFormat(format)

    MainWindow.show()
    val t = new QTimer()
    t.setInterval(3000)
    t.setSingleShot(true)
    t.timeout.connect(this, "hideSplash()")
    t.start()

    QApplication.exec()
  }

  private def hideSplash() {
    splash.close()
  }
}
