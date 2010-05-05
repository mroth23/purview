package org.purview.core.transforms

import org.purview.core.data.Matrix
import org.purview.core.data.MutableArrayMatrix
import scala.math._

case class Interpolate(dimX: Int, dimY: Int) extends Function1[Matrix[Float], Matrix[Float]] {
  def apply(matrix: Matrix[Float]): Matrix[Float] = {
    val result = new MutableArrayMatrix[Float](dimX, dimY)
    val sx = matrix.width / dimX.toFloat
    val sy = matrix.height / dimY.toFloat

    var x = 0
    while(x < dimX) {
      var y = 0
      while(y < dimY) {
        val oldX = sx * x;
        val oldY = sy * y;

        val x1 = oldX.toInt
        val x2 = x1 + 1
        val y1 = oldY.toInt
        val y2 = y1 + 1

        val q11 = matrix(x1, y1)
        val q21 = matrix(x2, y1)
        val q12 = matrix(x1, y2)
        val q22 = matrix(x2, y2)

        val dx = oldX - x1
        val dy = oldY - y1

        result(x, y) = q11 *      dx  *      dy  +
                       q21 * (1 - dx) *      dy  +
                       q12 *      dx  * (1 - dy) +
                       q22 * (1 - dx) * (1 - dy)
        y += 1
      }
      x += 1
    }

    result
  }
}