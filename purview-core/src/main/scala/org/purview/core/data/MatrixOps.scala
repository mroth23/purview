package org.purview.core.data

import java.awt.image.BufferedImage

object MatrixOps {
  def fragmentize[A : Manifest](in: Matrix[A], blockWidth: Int, blockHeight: Int): Matrix[Matrix[A]] = {
    val result = new Array[Matrix[A]]((in.width - blockWidth) * (in.height - blockHeight))
    val tmp = new Array[A](blockWidth * blockHeight)

    var y = 0
    while(y < in.height - blockHeight + 1) {
      var x = 0
      while(x < in.width - blockWidth) {
        var y1 = 0
        while(y1 < blockHeight) {
          var x1 = 0
          while(x1 < blockWidth) {
            tmp(x1 + y1 * blockWidth) = in(x + x1, y + y1)
            x1 += 1
          }
          y1 += 1
        }
        result(x + y * (in.width - blockWidth)) = new ImmutableMatrix(blockWidth, blockHeight, tmp.clone)
        x += 1
      }
      y += 1
    }
    new ImmutableMatrix(in.width - blockWidth, in.height - blockHeight, result)
  }

  def imageToMatrix(image: BufferedImage): Matrix[Color] = new Matrix[Color] {
    val width = image.getWidth
    val height = image.getHeight
    private val raster = image.getData
    private val buffer = new Array[Int](4)
    def apply(x: Int, y: Int) = synchronized {
      raster.getPixel(x, y, buffer)
      new Color(buffer(0), buffer(1), buffer(2), buffer(3))
    }
  }
}
