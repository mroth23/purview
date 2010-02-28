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
            tmp(x1 + y1 * blockWidth) = in.data((x + x1) + (y + y1) * in.width)
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

  def imageToMatrix(image: BufferedImage): Matrix[Color] = {
    val result = new MutableMatrix[Color](image.getWidth, image.getHeight)
    var y = 0
    while(y < image.getHeight) {
      var x = 0
      while(x < image.getWidth) {
        result(x, y) = MakeColor fromRGB image.getRGB(x, y)
        println(x)
        x += 1
      }
      y += 1
    }
    result
  }
}
