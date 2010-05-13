package org.purview.qtui

import com.trolltech.qt.core.Qt
import com.trolltech.qt.gui.QDockWidget
import com.trolltech.qt.gui.QIcon
import com.trolltech.qt.gui.QTextEdit
import java.io.OutputStream
import java.io.PrintStream

object LogView extends QDockWidget {
  setWindowTitle("Log output")
  setWindowIcon(QIcon.fromTheme("utilities-log-viewer", new QIcon("classpath:icons/utilities-log-viewer.png")))

  protected val logMessageArrived = new Signal1[String]

  protected def appendMessage(message: String) = {
    outputPane.append(message)
  }

  private val outputPane = new QTextEdit {
    setReadOnly(true)
  }

  setWidget(outputPane)

  private class InterceptorStream(interceptor: String => Any, out: OutputStream) extends OutputStream {
    override def write(i: Int): Unit = {
      interceptor(i.toChar.toString)
      out.write(i)
    }
    override def write(buf: Array[Byte], off: Int, len: Int): Unit = {
      val s = new String(buf, off, len)
      interceptor(s)
      out.write(buf, off, len)
    }
  }

  //System.setOut(new PrintStream(new InterceptorStream(logMessageArrived.emit, System.out), true, "UTF-8"))
  //System.setErr(new PrintStream(new InterceptorStream(x => logMessageArrived.emit("<em>" + x + "</em>"), System.err), true, "UTF-8"))
  //logMessageArrived.connect(this, "appendMessage(String)")
  //println("Started console output interceptor")
}
