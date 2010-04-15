package org.purview.core.transforms

import org.purview.core.data.ImmutableMatrix
import org.purview.core.data.Matrix

case class Fragmentize[@specialized(Int, Float) A : Manifest]
    (blockWidth: Int, blockHeight: Int) extends Function1[Matrix[A], Matrix[Matrix[A]]] {
  def apply(in: Matrix[A]) = {
    val result = new Array[Matrix[A]]((in.width - blockWidth) * (in.height - blockHeight))
    val tmp = new Array[A](blockWidth * blockHeight)

    var y = 0
    while(y < in.height - blockHeight) {
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
}
