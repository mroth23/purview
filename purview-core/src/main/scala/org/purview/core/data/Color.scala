package org.purview.core.data

import java.awt.{Color => AWTColor}
import scala.math._

/**
 * Common color definitions.
 */
object Color {

  /** Produces a new color with the given values */
  def apply(a: Float, r: Float, g: Float, b: Float) = new Color(a, r, g, b)

  /** Extracts color information from the given color */
  def unapply(c: Color) = Some((c.a, c.r, c.g, c.b))

  /** Produces a new color from the specified color array. Colors should have the order "RGBA" */
  def fromArray(colors: Array[Float]) = new Color(colors(3), colors(0), colors(1), colors(2))

  /** Produces a new color from the given java.awt.Color */
  def fromAWT(color: AWTColor) = fromArray(color.getRGBComponents(null))

  /** Produces a new color from the given ARGB integer value */
  def fromRGB(rgb: Int) = Color((rgb >>> 24) * 1f / 255f, ((rgb >>> 16) & 255) * 1f / 255f,
                               ((rgb >>> 8) & 255) * 1f / 255f, (rgb &   255) * 1f / 255f)

  /** The first primary spectrum component */
  val Red = new Color(1, 1, 0, 0) {
    override def toHTML = "red"
    override def toAWTColor = AWTColor.red
    override def toRGB = AWTColor.red.getRGB
  }

  /** The second primary spectrum component */
  val Green = new Color(1, 0, 1, 0) {
    override def toHTML = "green"
    override def toAWTColor = AWTColor.green
    override def toRGB = AWTColor.green.getRGB
  }

  /** The third primary spectrum component */
  val Blue = new Color(1, 0, 0, 1) {
    override def toHTML = "blue"
    override def toAWTColor = AWTColor.blue
    override def toRGB = AWTColor.blue.getRGB
  }

  /** The additive one in the spectrum */
  val White = new Color(1, 1, 1, 1) {
    override def toHTML = "white"
    override def toAWTColor = AWTColor.white
    override def toRGB = AWTColor.white.getRGB
  }

  /** The additive three-quarter in the spectrum */
  val LightGray = new Color(1, 0.75f, 0.75f, 0.75f) {
    override def toHTML = "lightgray"
    override def toAWTColor = AWTColor.lightGray
    override def toRGB = AWTColor.lightGray.getRGB
  }

  /** The additive half in the spectrum */
  val Gray = new Color(1, 0.5f, 0.5f, 0.5f) {
    override def toHTML = "gray"
    override def toAWTColor = AWTColor.gray
    override def toRGB = AWTColor.gray.getRGB
  }

  /** The additive quarter in the spectrum */
  val DarkGray = new Color(1, 0.25f, 0.25f, 0.25f) {
    override def toHTML = "darkgray"
    override def toAWTColor = AWTColor.darkGray
    override def toRGB = AWTColor.darkGray.getRGB
  }

  /** The additive zero in the spectrum */
  val Black = new Color(1, 0, 0, 0) {
    override def toHTML = "black"
    override def toAWTColor = AWTColor.black
    override def toRGB = AWTColor.black.getRGB
  }

  /** A hich-amplitude red tone */
  val Pink = new Color(1, 1, 0.6862745f, 0.6862745f) {
    override def toHTML = "pink"
    override def toAWTColor = AWTColor.pink
    override def toRGB = AWTColor.pink.getRGB
  }

  /** A weighted normalized addition between Red and Green */
  val Orange = new Color(1, 1, 0.78431374f, 0) {
    override def toHTML = "orange"
    override def toAWTColor = AWTColor.orange
    override def toRGB = AWTColor.orange.getRGB
  }

  /** A normalized addition between Red and Green */
  val Yellow = new Color(1, 1, 1, 0) {
    override def toHTML = "yellow"
    override def toAWTColor = AWTColor.yellow
    override def toRGB = AWTColor.yellow.getRGB
  }

  /** A normalized addition between Red and Blue */
  val Magenta = new Color(1, 1, 0, 1) {
    override def toHTML = "magenta"
    override def toAWTColor = AWTColor.magenta
    override def toRGB = AWTColor.magenta.getRGB
  }

  /** A normalized addition between Green and Blue */
  val Cyan = new Color(1, 0, 1, 1) {
    override def toHTML = "cyan"
    override def toAWTColor = AWTColor.cyan
    override def toRGB = AWTColor.cyan.getRGB
  }
}

/**
 * Defines a point in the color spectrum via an aplha, red, green and blue value.
 * All values have to be in the range 0f..1f for them to be visible. All other values
 * may only be rendered via HDR.
 * @param a The color intensity value
 * @param r The red color component
 * @param g The green color component
 * @param b The blue color component
 */
class Color(val a: Float, val r: Float, val g: Float, val b: Float) extends Product with NotNull {
  /** @see a */
  @inline final def alpha = a
  /** @see r */
  @inline final def red = r
  /** @see g */
  @inline final def green = g
  /** @see b */
  @inline final def blue = b

  /** The color intensity value, clamped and scaled between 0..255 */
  final def alphaByte = mkByte(a * 255 toInt)
  /** The red color component, clamped and scaled between 0..255 */
  final def redByte   = mkByte(r * 255 toInt)
  /** The green color component, clamped and scaled between 0..255 */
  final def greenByte = mkByte(g * 255 toInt)
  /** The blue color component, clamped and scaled between 0..255 */
  final def blueByte  = mkByte(b * 255 toInt)

  @inline final def toTuple = (a, r, g, b)
  def toAWTColor = new AWTColor(r, g, b, a)
  def toRGB = alphaByte << 24 | redByte << 16 | greenByte << 8 | blueByte
  def toHTML = '#' + padHex(redByte) + padHex(greenByte) + padHex(blueByte)

  def +(that: Color) = Color(this.a + that.a, this.r + that.r, this.g + that.g, this.b + that.b)
  def -(that: Color) = Color(this.a - that.a, this.r - that.r, this.g - that.g, this.b - that.b)
  def *(that: Color) = Color(this.a * that.a, this.r * that.r, this.g * that.g, this.b * that.b)
  def /(that: Color) = Color(this.a / that.a, this.r / that.r, this.g / that.g, this.b / that.b)
  def *(scale: Float) = Color(this.a * scale, this.r * scale, this.g * scale, this.b * scale)
  def /(scale: Float) = Color(this.a / scale, this.r / scale, this.g / scale, this.b / scale)

  def abs = Color(a.abs, r.abs, g.abs, b.abs)

  @inline final def weight = sqrt(a * a + r * r + g * g + b * b).toFloat

  @inline private def padHex(x: Int) = {
    val hex = Integer.toHexString(x)
    if(hex.length == 1)
      '0' + hex
    else
      hex
  }

  @inline private def mkByte(x: Int): Int =
    if(x < 0)
      0
    else if(x > 255)
      255
    else
      x

  def productArity = 4

  override def productPrefix = "Color"

  def productElement(i: Int) = i match {
    case 0 => a
    case 1 => r
    case 2 => g
    case 3 => b
    case _ => throw new NoSuchElementException
  }

  def canEqual(x: Any) = x.isInstanceOf[Color]

  override def equals(x: Any) = x match {
    case that: Color => this.a == that.a && this.r == that.r &&
      this.g == that.g && this.b == that.b
    case _ => false
  }

  override def hashCode = a.hashCode ^ r.hashCode ^ g.hashCode ^ b.hashCode
}
