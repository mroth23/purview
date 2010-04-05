package org.purview.qtui

import com.trolltech.qt.core.QObject
import com.trolltech.qt.core.QSignalMapper
import com.trolltech.qt.gui.QCheckBox
import com.trolltech.qt.gui.QDialog
import com.trolltech.qt.gui.QDialogButtonBox
import com.trolltech.qt.gui.QDoubleSpinBox
import com.trolltech.qt.gui.QFormLayout
import com.trolltech.qt.gui.QIcon
import com.trolltech.qt.gui.QLabel
import com.trolltech.qt.gui.QPalette
import com.trolltech.qt.gui.QSpinBox
import com.trolltech.qt.gui.QToolBox
import com.trolltech.qt.gui.QVBoxLayout
import com.trolltech.qt.gui.QWidget
import org.purview.core.analysis.Analyser
import org.purview.core.analysis.Settings
import org.purview.core.analysis.settings.FloatRangeSetting
import org.purview.core.analysis.settings.IntRangeSetting
import org.purview.core.analysis.settings.Setting
import org.purview.core.data.ImageMatrix
import org.purview.qtui.meta.ImageSession

class SettingsDialog(session: ImageSession, parent: QWidget = null) extends QDialog(parent) {
  setWindowTitle("Configure analysers")
  setWindowIcon(new QIcon("classpath:icons/configure.png"))
  setFixedSize(500, 300)
  
  private val orderedAnalysers = session.analysers.keySet.toSeq.sortWith(_.name < _.name)

  private var toggleAnalyserCallbacks: Map[QObject, Analyser[ImageMatrix]] = Map.empty
  
  private val toggleAnalyserMapper = new QSignalMapper(this) {
    mappedQObject.connect(SettingsDialog.this, "toggleAnalyser(QObject)")
  }

  private var settingsCallbacks: Map[QObject, Setting[_]] = Map.empty

  private val settingsMapper = new QSignalMapper(this) {
    mappedQObject.connect(SettingsDialog.this, "changeSetting(QObject)")
  }

  val analyserPages: Map[Analyser[ImageMatrix], QWidget] = orderedAnalysers partialMap {
    case analyserWithSettings: Settings =>
      val page = new QWidget {
        val p = palette
        val c = p.color(QPalette.ColorRole.Window)
        c.setAlpha(0)
        p.setColor(QPalette.ColorRole.Window, c)
        setPalette(p)
        val form = new QFormLayout(this)
        for(setting <- analyserWithSettings.settings) {
          val widget = setting match {
            case f: FloatRangeSetting =>
              val spinner = new QDoubleSpinBox(this) {
                setDecimals(Math.floor(Math.log(f.granularity) / Math.log(10)).toInt)
                setSingleStep(1 / f.granularity)
                setRange(f.min, f.max)
                setValue(f.value)
              }
              settingsMapper.setMapping(spinner, spinner)
              settingsCallbacks += spinner -> f
              spinner.valueChanged.connect(settingsMapper, "map()")
              spinner
            case i: IntRangeSetting =>
              val spinner = new QSpinBox(this) {
                setRange(i.min, i.max)
                setValue(i.value)
              }
              settingsMapper.setMapping(spinner, spinner)
              settingsCallbacks += spinner -> i
              spinner.valueChanged.connect(settingsMapper, "map()")
              spinner
            case _ =>
              new QLabel("<em>Unsupported setting</em>")
          }
          form.addRow(setting.name, widget)
        }
        setLayout(form)
      }
      analyserWithSettings -> page
  } toMap

  val analyserCheckboxes = for(analyser <- orderedAnalysers) yield {
    val enabled = session.analysers(analyser)
    val checkBox = new QCheckBox(this)
    checkBox.setChecked(enabled)
    toggleAnalyserMapper.setMapping(checkBox, checkBox)
    toggleAnalyserCallbacks += checkBox -> analyser
    checkBox.toggled.connect(toggleAnalyserMapper, "map()")
    checkBox
  }

  private val toolBox = new QToolBox(this) {
    val selectAnalysersPage = new QWidget {
      val p = palette
      val c = p.color(QPalette.ColorRole.Window)
      c.setAlpha(0)
      p.setColor(QPalette.ColorRole.Window, c)
      setPalette(p)
      val form = new QFormLayout(this)
      (orderedAnalysers zip analyserCheckboxes).foreach(x => form.addRow(x._1.name, x._2))
      setLayout(form)
    }
    addItem(selectAnalysersPage, new QIcon("classpath:icons/checkbox.png"), "Active analysers")
    for {
      analyser <- orderedAnalysers
      page <- analyserPages.get(analyser)
    } addItem(page, new QIcon("classpath:" + (analyser.iconResource getOrElse "icons/system-run.png")), analyser.name)
  }

  private val buttons = new QDialogButtonBox(this) {
    setStandardButtons(QDialogButtonBox.StandardButton.Ok)
    button(QDialogButtonBox.StandardButton.Ok).setDefault(true)
    accepted.connect(SettingsDialog.this, "accept()")
  }
  
  private val vBoxLayout = new QVBoxLayout(this) {
    addWidget(toolBox)
    addWidget(buttons)
  }
  
  setLayout(vBoxLayout)

  private def toggleAnalyser(sender: QObject) = {
    val analyser = toggleAnalyserCallbacks(sender)
    val checkBox = sender.asInstanceOf[QCheckBox]
    val enabled = checkBox.isChecked
    session.analysers += analyser -> enabled
    analyserPages.get(analyser) foreach { page =>
      toolBox.setItemEnabled(toolBox.indexOf(page), enabled)
    }
  }

  private def changeSetting(sender: QObject) = {
    val setting: Setting[_] = settingsCallbacks(sender)
    setting match {
      case f: FloatRangeSetting =>
        val spinner = sender.asInstanceOf[QDoubleSpinBox]
        f.value = spinner.value.toFloat
      case i: IntRangeSetting =>
        val spinner = sender.asInstanceOf[QSpinBox]
        i.value = spinner.value
      case _ => error("FATAL: Unsupported setting field")
    }
  }
}
