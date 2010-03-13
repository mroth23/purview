package org.purview.core.transforms

import java.awt.image.BufferedImage
import org.purview.core.data.Color
import org.purview.core.data.Matrix

case class MatrixToImage() extends Function1[Matrix[Color], BufferedImage] {
  def apply(input: Matrix[Color]) = {
    val result = new BufferedImage(input.width, input.height, BufferedImage.TYPE_INT_ARGB)
    var y = 0
    while(y < input.height) {
      var x = 0
      while(x < input.width) {
        result.setRGB(x, y, input(x, y).toRGB)
        x += 1
      }
      y += 1
    }
    result
  }
}
