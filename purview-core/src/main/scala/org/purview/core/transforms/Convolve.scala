package org.purview.core.transforms

import org.purview.core.data.Matrix
import org.purview.core.data.MutableArrayMatrix
import scala.math._

case class Convolve(kernel: Matrix[Float]) extends Function1[Matrix[Float], Matrix[Float]] {
  def apply(input: Matrix[Float]) = {
    val result = new MutableArrayMatrix[Float](input.width, input.height)

    val kWidth = kernel.width
    val kHeight = kernel.height
    val kCenterX = (kWidth - 1) / 2
    val kCenterY = (kHeight - 1) / 2

    val rWidth = input.width
    val rHeight = input.height

    @inline def between(x: Int, low: Int, high: Int) = if(x < low) low else if(x > high) high else x

    var x = 0
    while(x < rWidth) {
      var y = 0
      while(y < rHeight) {
        var dx = 0
        var value = 0.0f
        while(dx < kWidth) {
          var dy = 0
          while(dy < kHeight) {
            value += kernel(dx, dy) * input(between(x + dx - kCenterX, 0, rWidth - 1), between(y + dy - kCenterY, 0, rHeight - 1))
            dy += 1
          }
          dx += 1
        }
        result(x, y) = value
        y += 1
      }
      x += 1
    }

    result
  }
}
