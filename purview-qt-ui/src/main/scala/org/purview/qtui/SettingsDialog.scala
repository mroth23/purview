package org.purview.qtui

import com.trolltech.qt.QSignalEmitter
import com.trolltech.qt.core.QObject
import com.trolltech.qt.core.QSignalMapper
import com.trolltech.qt.core.Qt
import com.trolltech.qt.gui.QCheckBox
import com.trolltech.qt.gui.QDialog
import com.trolltech.qt.gui.QDialogButtonBox
import com.trolltech.qt.gui.QDoubleSpinBox
import com.trolltech.qt.gui.QFormLayout
import com.trolltech.qt.gui.QIcon
import com.trolltech.qt.gui.QLabel
import com.trolltech.qt.gui.QLayout
import com.trolltech.qt.gui.QPalette
import com.trolltech.qt.gui.QSpinBox
import com.trolltech.qt.gui.QSlider
import com.trolltech.qt.gui.QToolBox
import com.trolltech.qt.gui.QHBoxLayout
import com.trolltech.qt.gui.QVBoxLayout
import com.trolltech.qt.gui.QWidget
import org.purview.core.analysis.Analyser
import org.purview.core.analysis.Settings
import org.purview.core.analysis.settings.BooleanSetting
import org.purview.core.analysis.settings.FloatRangeSetting
import org.purview.core.analysis.settings.IntRangeSetting
import org.purview.core.data.ImageMatrix
import org.purview.qtui.meta.ImageSession
import org.purview.qtui.widgets.QDoubleSlider

class SettingsDialog(session: ImageSession, parent: QWidget = null) extends QDialog(parent) {
  setWindowTitle("Configure analysers")
  setWindowIcon(QIcon.fromTheme("configure", new QIcon("classpath:icons/configure.png")))
  setFixedSize(500, 300)

  private val orderedAnalysers = session.analysers.keySet.toSeq.sortWith(_.name < _.name)

  val analyserPages: Map[Analyser[ImageMatrix], QWidget] = orderedAnalysers collect {
    case analyserWithSettings: Settings =>
      val page = new QWidget {
        val p = palette
        val c = p.color(QPalette.ColorRole.Window)
        c.setAlpha(0)
        p.setColor(QPalette.ColorRole.Window, c)
        setPalette(p)
        val form = new QFormLayout(this)
        for(setting <- analyserWithSettings.settings) {
          val entry: Either[QLayout, QWidget] = setting match {
            case f: FloatRangeSetting =>
              val layout = new QHBoxLayout(this)
              val spinner = new QDoubleSpinBox(this) {
                setDecimals(math.floor(math.log(f.granularity) / math.log(10)).toInt)
                setSingleStep(1 / f.granularity)
                setRange(f.min, f.max)
                setValue(f.value)
                valueChanged.connect(this, "changeSetting()")
                def changeSetting() = f.value = this.value.toFloat
              }
              val slider = new QDoubleSlider(this) {
                setOrientation(Qt.Orientation.Horizontal)
                setGranularity(f.granularity)
                setDoubleRange(f.min, f.max)
                setDoubleValue(f.value)
              }
              spinner.valueChanged.connect(slider, "setDoubleValue(double)")
              slider.doubleValueChanged.connect(spinner, "setValue(double)")
              layout.addWidget(spinner)
              layout.addWidget(slider)
              Left(layout)
            case i: IntRangeSetting =>
              val layout = new QHBoxLayout(this)
              val spinner = new QSpinBox(this) {
                setRange(i.min, i.max)
                setValue(i.value)
                valueChanged.connect(this, "changeSetting()")
                def changeSetting() = (i.value = this.value)
              }
              val slider = new QSlider(this) {
                setOrientation(Qt.Orientation.Horizontal)
                setRange(i.min, i.max)
                setValue(i.value)
              }
              spinner.valueChanged.connect(slider, "setValue(int)")
              slider.valueChanged.connect(spinner, "setValue(int)")
              layout.addWidget(spinner)
              layout.addWidget(slider)
              Left(layout)
            case b: BooleanSetting =>
              Right(new QCheckBox(this) {
                  setChecked(b.value)
                  this.toggled.connect(this, "changeSetting()")
                  def changeSetting() = (b.value = this.isChecked)
                })
            case _ =>
              Right(new QLabel("<em>Unsupported setting</em>"))
          }
          entry match {
            case Right(widget) =>
              form.addRow(setting.name, widget)
            case Left(layout) =>
              form.addRow(setting.name, layout)
          }
        }
        setLayout(form)
      }
      analyserWithSettings -> page
  } toMap

  val analyserCheckboxes = for(analyser <- orderedAnalysers) yield 
    new QCheckBox(this) {
      setChecked(session.analysers(analyser))
      toggled.connect(this, "toggleAnalyser()")
      def toggleAnalyser() = {
        session.analysers = session.analysers.updated(analyser, isChecked)
        analyserPages.get(analyser) foreach { page =>
          toolBox.setItemEnabled(toolBox.indexOf(page), isChecked)
        }
      }
    }

  private val toolBox: QToolBox = new QToolBox(this) {
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
    addItem(selectAnalysersPage, QIcon.fromTheme("checkbox", new QIcon("classpath:icons/checkbox.png")), "Active analysers")
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
}
