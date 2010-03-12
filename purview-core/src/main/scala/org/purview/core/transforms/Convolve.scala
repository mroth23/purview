package org.purview.core.transforms

import org.purview.core.data.Matrix
import org.purview.core.data.MutableMatrix
import scala.math._

case class Convolve(kernel: Matrix[Float]) extends Function1[Matrix[Float], Matrix[Float]] {
  def apply(input: Matrix[Float]) = {
    val result = new MutableMatrix[Float](input.width, input.height)

    val kwidth = kernel.width
    val kheight = kernel.height

    val left = (kwidth - 1) / 2
    val right = max((kwidth - left - 1), 0)
    val top = (kheight - 1) / 2
    val bottom = max((kheight - top - 1), 0)

    val rwidth = input.width - left - right
    val rheight = input.height - top - bottom

    var x = 0
    while(x < rwidth) {
      var y = 0
      while(y < rheight) {
        var dx = 0
        var value: Float = 0
        while(dx < kwidth) {
          var dy = 0
          while(dy < kheight) {
            value += kernel(dx, dy) * input(x + dx, y + dy)
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
