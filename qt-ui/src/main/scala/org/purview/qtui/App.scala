package org.purview.qtui

import com.trolltech.qt.core.QEventLoop
import com.trolltech.qt.core.QTimer
import com.trolltech.qt.core.Qt
import com.trolltech.qt.gui.QApplication
import com.trolltech.qt.gui.QPaintEvent
import com.trolltech.qt.gui.QPainter
import com.trolltech.qt.gui.QPixmap
import com.trolltech.qt.gui.QWidget

object App {
  private class SplashScreen(pixmap: QPixmap) extends QWidget {
    setWindowFlags(Qt.WindowType.FramelessWindowHint)
    setWindowFlags(Qt.WindowType.SplashScreen)
    setAttribute(Qt.WidgetAttribute.WA_TranslucentBackground)
    setAutoFillBackground(false)
    setWindowTitle("Loading Purview 1.0")
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
    val eventLoop = QEventLoop.fromNativePointer(QApplication.instance.nativePointer)
    eventLoop.processEvents()

    MainWindow.show()
    val t = new QTimer()
    t.setInterval(2000)
    t.setSingleShot(true)
    t.timeout.connect(this, "hideSplash()")
    t.start()

    QApplication.exec()
    System.exit(0)
  }

  private def hideSplash() {
    splash.close()
  }
}
