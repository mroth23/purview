package org.purview.core.data

import java.awt.image.BufferedImage

/**
 * A matrix that is specialized for images, and stores values very efficiently
 */
case class ImageMatrix(image: BufferedImage, metadata: Map[String, Map[String, String]]) extends Matrix[Color] {

    val width = image.getWidth
    val height = image.getHeight
    private val raster = image.getData
    private val buffer = new Array[Int](4)
    def apply(x: Int, y: Int) = synchronized {
      raster.getPixel(x, y, buffer)
      new Color(buffer(3) / 255f, buffer(0) / 255f, buffer(1) / 255f, buffer(2) / 255f)
    }
}
