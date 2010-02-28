package org.purview.core.data

import java.awt.{Color => AWTColor}

object MakeColor {
  def fromArray(colors: Array[Float]) = new Color(colors(0), colors(1), colors(2), colors(3))

  def fromAWT(color: AWTColor) = fromArray(color.getRGBComponents(null))
  def fromRGB(rgb: Int) = Color((rgb >>> 24) * 1f / 255f, ((rgb >>> 16) & 255) * 1f / 255f,
                                ((rgb >>> 8) & 255) * 1f / 255f, (rgb &   255) * 1f / 255f)
}

object Colors {
  val Red = new Color(1, 1, 0, 0) {
    override def toHTML = "red"
    override def toAWTColor = AWTColor.red
    override def toRGB = AWTColor.red.getRGB
  }

  val Green = new Color(1, 0, 1, 0) {
    override def toHTML = "green"
    override def toAWTColor = AWTColor.green
    override def toRGB = AWTColor.green.getRGB
  }

  val Blue = new Color(1, 0, 0, 1) {
    override def toHTML = "blue"
    override def toAWTColor = AWTColor.blue
    override def toRGB = AWTColor.blue.getRGB
  }

  val White = new Color(1, 1, 1, 1) {
    override def toHTML = "white"
    override def toAWTColor = AWTColor.white
    override def toRGB = AWTColor.white.getRGB
  }
  
  val LightGray = new Color(1, 0.75f, 0.75f, 0.75f) {
    override def toHTML = "lightgray"
    override def toAWTColor = AWTColor.lightGray
    override def toRGB = AWTColor.lightGray.getRGB
  }

  val Gray = new Color(1, 0.5f, 0.5f, 0.5f) {
    override def toHTML = "gray"
    override def toAWTColor = AWTColor.gray
    override def toRGB = AWTColor.gray.getRGB
  }

  val DarkGray = new Color(1, 0.25f, 0.25f, 0.25f) {
    override def toHTML = "darkgray"
    override def toAWTColor = AWTColor.darkGray
    override def toRGB = AWTColor.darkGray.getRGB
  }

  val Black = new Color(1, 0, 0, 0) {
    override def toHTML = "black"
    override def toAWTColor = AWTColor.black
    override def toRGB = AWTColor.black.getRGB
  }

  val Pink = new Color(1, 1, 0.6862745f, 0.6862745f) {
    override def toHTML = "pink"
    override def toAWTColor = AWTColor.pink
    override def toRGB = AWTColor.pink.getRGB
  }

  val Orange = new Color(1, 1, 0.78431374f, 0) {
    override def toHTML = "orange"
    override def toAWTColor = AWTColor.orange
    override def toRGB = AWTColor.orange.getRGB
  }

  val Yellow = new Color(1, 1, 1, 0) {
    override def toHTML = "yellow"
    override def toAWTColor = AWTColor.yellow
    override def toRGB = AWTColor.yellow.getRGB
  }

  val Magenta = new Color(1, 1, 0, 1) {
    override def toHTML = "magenta"
    override def toAWTColor = AWTColor.magenta
    override def toRGB = AWTColor.magenta.getRGB
  }

  val Cyan = new Color(1, 0, 1, 1) {
    override def toHTML = "cyan"
    override def toAWTColor = AWTColor.cyan
    override def toRGB = AWTColor.cyan.getRGB
  }
}

case class Color(a: Float, r: Float, g: Float, b: Float) extends NotNull {
  def alpha = a
  def red = r
  def green = g
  def blue = b

  def alphaByte = mkByte((a * 255 toInt) << 24)
  def redByte   = mkByte((r * 255 toInt) << 16)
  def greenByte = mkByte((g * 255 toInt) << 8 )
  def blueByte  = mkByte((b * 255 toInt)      )
  
  def toTuple = (a, r, g, b)
  def toAWTColor = new AWTColor(a, r, g, b)
  def toRGB = alphaByte << 24 | redByte << 16 | greenByte << 8 | blueByte
  def toHTML = '#' + padHex(redByte) + padHex(greenByte) + padHex(blueByte)
  
  def +(that: Color) = Color(this.a + that.a, this.r + that.r, this.g + that.g, this.b + that.b)
  def -(that: Color) = Color(this.a - that.a, this.r - that.r, this.g - that.g, this.b - that.b)
  def *(that: Color) = Color(this.a * that.a, this.r * that.r, this.g * that.g, this.b * that.b)
  def /(that: Color) = Color(this.a / that.a, this.r / that.r, this.g / that.g, this.b / that.b)
  def *(scale: Float) = Color(this.a * scale, this.r * scale, this.g * scale, this.b * scale)
  def /(scale: Float) = Color(this.a / scale, this.r / scale, this.g / scale, this.b / scale)

  def abs = Color(Math.abs(a), Math.abs(r), Math.abs(g), Math.abs(b))

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
}
