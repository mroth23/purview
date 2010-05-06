package org.purview.core.transforms

import org.purview.core.data.Matrix
import org.purview.core.data.MutableArrayMatrix
import scala.math._

case class Interpolate(dimX: Int, dimY: Int) extends Function1[Matrix[Float], Matrix[Float]] {
  def apply(matrix: Matrix[Float]): Matrix[Float] = {

    val result = new MutableArrayMatrix[Float](dimX, dimY)
    val sx = matrix.width.toFloat / dimX.toFloat
    val sy = matrix.height.toFloat / dimY.toFloat

    var x = 0
    while(x < dimX) {
      var y = 0
      while(y < dimY) {

        val fx = floor(sx * x.toFloat).toInt //floor x
        val fy = floor(sy * y.toFloat).toInt //floor y
        var cx = fx + 1 //ceiling x
        if (cx >= matrix.width) cx = fx
        var cy = fy + 1 //ceiling y
        if (cy >= matrix.height) cy = fy
        val frac_x = (x.toFloat * sx) - fx.toFloat
        val frac_y = (y.toFloat * sy) - fy.toFloat

        val p1 = matrix(fx, fy)
        val p2 = matrix(cx, fy)
        val p3 = matrix(fx, cy)
        val p4 = matrix(cx, cy)

        result(x, y) = (1.0f - frac_y) * ((1.0f - frac_x) * p1 + frac_x * p2) +
        frac_y * ((1.0f - frac_x) * p3 + frac_x * p4)
        
        y += 1
      }
      x += 1
    }

    result
  }
}