package org.purview.core.transforms

import java.awt.image.BufferedImage
import org.purview.core.data.Color
import org.purview.core.data.Matrix

case class ImageToMatrix() extends Function1[BufferedImage, Matrix[Color]] {
  def apply(image: BufferedImage) = new Matrix[Color] {
    val width = image.getWidth
    val height = image.getHeight
    private val raster = image.getData
    private val buffer = new Array[Int](4)
    def apply(x: Int, y: Int) = synchronized {
      raster.getPixel(x, y, buffer)
      new Color(buffer(3) / 255f, buffer(0) / 255f, buffer(1) / 255f, buffer(2) / 255f)
    }
  }
}
