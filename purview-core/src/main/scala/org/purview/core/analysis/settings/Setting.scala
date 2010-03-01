package org.purview.core.analysis.settings

abstract class Setting[A] {
  val name: String
  var value: A
}

case class IntRangeSetting(name: String, val min: Int, val max: Int) extends Setting[Int] {
  private var v = min

  def value = v
  def value_=(f: Int): Unit = {
    if(value > max || value < min)
      error("Setting out of range")
    else
      v = f
  }
}

case class FloatRangeSetting(name: String, val min: Float, val max: Float) extends Setting[Float] {
  private var v = min

  def value = v
  def value_=(f: Float): Unit = {
    if(value > max || value < min)
      error("Setting out of range")
    else
      v = f
  }
}
