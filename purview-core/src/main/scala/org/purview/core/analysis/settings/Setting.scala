package org.purview.core.analysis.settings

/**
 * A setting that can be changed
 */
abstract class Setting[@specialized A] {
  /** The name of this setting */
  val name: String

  /** This setting's value */
  var value: A
}

/**
 * An integer setting that is limited by two values
 * @param name The name of this setting
 * @param min The minimum accepted value
 * @param max The maximum accepted value
 */
case class IntRangeSetting(name: String, min: Int, max: Int) extends Setting[Int] {
  private var v = min

  def value = v
  def value_=(f: Int): Unit = {
    if(value > max || value < min)
      throw new IllegalArgumentException("Setting out of range: " + f)
    else
      v = f
  }
}

/**
 * A float setting that is limited by two values
 * @param name The name of this setting
 * @param min The minimum accepted value
 * @param max The maximum accepted value
 * @param granularity The "resolution" of the setting. 1/granularity will be the
 *   smallest supported setting increment.
 */
case class FloatRangeSetting(name: String, min: Float, max: Float, granularity: Int = 100) extends Setting[Float] {
  private var v = min

  def value = v
  def value_=(f: Float): Unit = {
    if(value > max || value < min)
      throw new IllegalArgumentException("Setting out of range: " + f)
    else
      v = f
  }
}

case class BooleanSetting(name: String) extends Setting[Boolean] {
  var value = false
}
