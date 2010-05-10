package org.purview.qtui

import com.trolltech.qt.core.Qt
import com.trolltech.qt.gui.QDockWidget
import com.trolltech.qt.gui.QFormLayout
import com.trolltech.qt.gui.QIcon
import com.trolltech.qt.gui.QLabel
import com.trolltech.qt.gui.QMessageBox
import com.trolltech.qt.gui.QProgressBar
import com.trolltech.qt.gui.QTextEdit
import com.trolltech.qt.gui.QVBoxLayout
import com.trolltech.qt.gui.QWidget
import org.purview.qtui.meta.Analysis
import scala.collection.mutable.WeakHashMap

object AnalysisView extends QDockWidget {
  setWindowTitle("Analysis")
  setWindowIcon(QIcon.fromTheme("preferences-system-session-services", new QIcon("classpath:icons/preferences-system-session-services.png")))
  setAllowedAreas(Qt.DockWidgetArea.BottomDockWidgetArea, Qt.DockWidgetArea.TopDockWidgetArea)

  private val widgetForAnalysis = new WeakHashMap[Option[Analysis], QWidget]
  widgetForAnalysis(None) = mkWidget(None)
  setWidget(widgetForAnalysis(None))

  private var _analysis: Option[Analysis] = None
  def analysis = _analysis
  def analysis_=(maybeAnalysis: Option[Analysis]) = {
    setWidget(widgetForAnalysis.getOrElseUpdate(maybeAnalysis,
                                                mkWidget(maybeAnalysis)))
    _analysis = maybeAnalysis
  }

  def mkWidget(analysis: Option[Analysis]) = new QWidget(this) {
    private val statusLabel = new QTextEdit(this) {
      setReadOnly(true)
    }
    private val analyserLabel = new QLabel(this)
    private val progressBar = new QProgressBar(this) {
      setRange(0, 100)
    }
    private val subProgressBar = new QProgressBar(this) {
      setRange(0, 100)
    }
    private val boxLayout = new QVBoxLayout(this) {
      private val labelForm = new QFormLayout {
        addRow("Analyser", analyserLabel)
        addRow("Status", statusLabel)
      }
      addLayout(labelForm)
      addWidget(progressBar)
      addWidget(subProgressBar)
    }
    setLayout(boxLayout)

    analysis.foreach { a =>
      a.progressChanged.connect(this, "setProgress(float)")
      a.subProgressChanged.connect(this, "setSubProgress(float)")
      a.statusChanged.connect(this, "setStatus(String)")
      a.analyserChanged.connect(this, "setAnalyser(String)")
      a.error.connect(this, "reportError(String)")
    }

    private def setProgress(progress: Float) =
      progressBar.setValue((progress * 100).toInt)

    private def setSubProgress(progress: Float) =
      subProgressBar.setValue((progress * 100).toInt)

    private def setStatus(status: String) =
      statusLabel.append(status)

    private def setAnalyser(analyser: String) =
      analyserLabel.setText(analyser)

    private def reportError(message: String) =
      QMessageBox.critical(this, "Error during analysis", message)
  }
}
