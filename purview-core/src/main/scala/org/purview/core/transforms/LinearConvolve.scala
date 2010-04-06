package org.purview.core.transforms

import org.purview.core.data.Matrix
import org.purview.core.data.MutableArrayMatrix
import scala.math._

case class LinearConvolve(kernel: Array[Float]) extends Function1[Matrix[Float], Matrix[Float]] {
  def apply(input: Matrix[Float]) = {
    val resultHoriz = new MutableArrayMatrix[Float](input.width, input.height)
    val resultVert = new MutableArrayMatrix[Float](input.width, input.height)

    val kLen = kernel.length
    val kCenter = (kLen - 1) / 2

    val rWidth = input.width
    val rHeight = input.height

    @inline def between(x: Int, low: Int, high: Int) = if(x < low) low else if(x > high) high else x

    var x = 0
    while(x < rWidth) {
      var y = 0
      while(y < rHeight) {
        var i = 0
        var value = 0.0f
        while(i < kLen) {
          value += kernel(i) * input(between(x + i - kCenter, 0, rWidth - 1), y)
          i += 1
        }
        resultHoriz(x, y) = value
        y += 1
      }
      x += 1
    }

    x = 0
    while(x < rWidth) {
      var y = 0
      while(y < rHeight) {
        var i = 0
        var value = 0.0f
        while(i < kLen) {
          value += kernel(i) * resultHoriz(x, between(y + i - kCenter, 0, rHeight - 1))
          i += 1
        }
        resultVert(x, y) = value
        y += 1
      }
      x += 1
    }

    resultVert
  }
}
