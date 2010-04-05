package org.purview.qtui.widgets

import com.trolltech.qt.gui.QSlider
import com.trolltech.qt.gui.QWidget

class QDoubleSlider(parent: QWidget = null) extends QSlider(parent) {
  private var min, max, v = 0d
  private var gran = 100

  val doubleValueChanged = new Signal1[Double]

  valueChanged.connect(this, "calculateDoubleValue(int)")

  def granularity = gran
  def granularity_=(value: Int) = setGranularity(value)
  def setGranularity(value: Int) = {
    gran = value
    setRange(0, gran)
  }

  def doubleValue: Double = v
  def doubleValue_=(value: Double) = setDoubleValue(value)
  def setDoubleValue(value: Double) = if(v != value) {
    v = value
    super.setValue(((value - min) * gran / (max - min)).toInt)
  }

  def setDoubleRange(min: Double, max: Double) = {
    this.min = min
    this.max = max
  }

  private def calculateDoubleValue(value: Int) = {
    doubleValueChanged.emit(value * (max-min) / gran + min)
  }
}
