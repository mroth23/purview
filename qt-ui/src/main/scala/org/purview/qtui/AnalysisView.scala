package org.purview.qtui

import com.trolltech.qt.core.Qt
import com.trolltech.qt.gui.QDockWidget
import com.trolltech.qt.gui.QIcon
import com.trolltech.qt.gui.QLabel
import com.trolltech.qt.gui.QProgressBar
import com.trolltech.qt.gui.QVBoxLayout
import com.trolltech.qt.gui.QWidget
import org.purview.qtui.meta.Analysis

object AnalysisView extends QDockWidget {
  setWindowTitle("Analysis")
  setWindowIcon(new QIcon("classpath:icons/preferences-system-session-services.png"))
  setAllowedAreas(Qt.DockWidgetArea.BottomDockWidgetArea, Qt.DockWidgetArea.TopDockWidgetArea)

  private val contents = new QWidget

  private val statusLabel = new QLabel(contents)
  
  private val analyserLabel = new QLabel(contents)

  private val progressBar = new QProgressBar(contents) {
    setRange(0, 100)
  }

  private val subProgressBar = new QProgressBar(contents) {
    setRange(0, 100)
  }

  private val boxLayout = new QVBoxLayout(contents) {
    addWidget(statusLabel)
    addWidget(analyserLabel)
    addWidget(progressBar)
    addWidget(subProgressBar)
  }

  setWidget(contents)

  private var _analysis: Option[Analysis] = None

  def analysis = _analysis

  def analysis_=(analysis: Option[Analysis]) = {
    _analysis match {
      case Some(a) =>
        a.statusChanged.disconnect(this)
        a.analyserChanged.disconnect(this)
        a.progressChanged.disconnect(this)
        a.subProgressChanged.disconnect(this)
      case _ =>
    }
    analysis match {
      case Some(ana) =>
        setWindowTitle("Analysis - " + ana.name)
        ana.progressChanged.connect(this, "setProgress(float)")
        ana.subProgressChanged.connect(this, "setSubProgress(float)")
        ana.statusChanged.connect(this, "setStatus(String)")
        ana.analyserChanged.connect(this, "setAnalyser(String)")
        progressBar.setDisabled(false)
        subProgressBar.setDisabled(false)
        setProgress(ana.progress)
        setSubProgress(ana.subProgress)
        setStatus(ana.status)
        setAnalyser(ana.analyser)
        _analysis = analysis
      case None =>
        setWindowTitle("Analysis")
        progressBar.reset()
        progressBar.setDisabled(true)
        subProgressBar.reset()
        subProgressBar.setDisabled(true)
        statusLabel.clear()
        analyserLabel.clear()
        _analysis = analysis
    }
  }

  private def setProgress(progress: Float) =
    progressBar.setValue((progress * 100).toInt)
  
  private def setSubProgress(progress: Float) =
    subProgressBar.setValue((progress * 100).toInt)

  private def setStatus(status: String) =
    statusLabel.setText("Status: " + status)

  private def setAnalyser(analyser: String) =
    analyserLabel.setText("Analyser: " + analyser)
}
