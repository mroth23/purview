package org.purview.core.transforms

import org.purview.core.data._
import scala.math._

case class DiscreteCosineTransform() extends Function1[Matrix[Float], Matrix[Float]] {

  def coefficients(bw: Int, bh: Int) : Matrix[Float] = new Matrix[Float] {
      val width = bw
      val height = bh

      private val Double = (sqrt(1f / bw) * sqrt(1f / bh)).toFloat
      private val Sans = (sqrt(2f / bw) * sqrt(2f / bh )).toFloat
      private val Single = (sqrt(Double) * sqrt(Sans)).toFloat

      //Don't actually store the matrix; select values on the fly
      def apply(x: Int, y: Int) = if(x == 0 && y == 0)
        Double
      else if(x == 1 || y == 1)
        Single
      else
        Sans
    }

  def apply(data: Matrix[Float]): Matrix[Float] = {
    val w = data.width
    val h = data.height
    val result = new MutableArrayMatrix[Float](w, h)
    val pi = 3.141592653589793f
    val coeff = coefficients(w, h)
    var v = 0
    while(v < w) {
      var u = 0
      while(u < h) {

        var sum = 0f
        //Actually calculate the local sum
        var y = 0
        while(y < h) {
          var x = 0
          while(x < w) {
            sum += data(x, y) * (cos(pi * u * (2.0 * x + 1.0) / (w * 2)) * cos(pi * v * (2.0 * y + 1.0) / (h * 2))).toFloat
            x += 1
          }
          y += 1
        }
        sum *= coeff(u, v)
        result(u, v) = sum
        u += 1
      }
      v += 1
    }
    result
  }

}

case class InverseDiscreteCosineTransform() extends Function1[Matrix[Float], Matrix[Float]] {

  def coefficients(bw: Int, bh: Int) : Matrix[Float] = new Matrix[Float] {
      val width = bw
      val height = bh

      private val Double = 1f / (sqrt(1f / bw) * sqrt(1f / bh)).toFloat
      private val Sans = 1f / (sqrt(2f / bw) * sqrt(2f / bh )).toFloat
      private val Single = 1f / (sqrt(Double) * sqrt(Sans)).toFloat

      //Don't actually store the matrix; select values on the fly
      def apply(x: Int, y: Int) = if(x == 0 && y == 0)
        Double
      else if(x == 1 || y == 1)
        Single
      else
        Sans
    }

  def apply(data: Matrix[Float]): Matrix[Float] = {
    val w = data.width
    val h = data.height
    val result = new MutableArrayMatrix[Float](w, h)
    val pi = 3.141592653589793f
    val coeff = coefficients(w, h)
    val dc = (1f / 2f) * data(0, 0)
    var v = 0
    while(v < w) {
      var u = 0
      while(u < h) {

        var sum = dc
        //Actually calculate the local sum
        var y = 0
        while(y < h) {
          var x = 0
          while(x < w) {
            sum += (data(x, y) * cos((pi / w) * x * (v - 1f / 2f)) * cos((pi / h) * y * (u - 1f / 2f))).toFloat
            x += 1
          }
          y += 1
        }
        sum *= coeff(u, v)
        result(u, v) = sum
        u += 1
      }
      v += 1
    }
    result
  }

}
