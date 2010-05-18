package org.purview.core.transforms

import org.purview.core.data._
import scala.math._

case class DiscreteCosineTransform() extends Function1[Matrix[Float], Matrix[Float]] {

  def coefficients(bw: Int, bh: Int) : Matrix[Float] = new Matrix[Float] {
    val width = bw
    val height = bh

    private val Double = (sqrt(1f / bw) * sqrt(1f / bh)).toFloat
    private val Sans = (sqrt(2f / bw) * sqrt(2f / bh )).toFloat
    private val SingleX = (sqrt(1f / bw) * sqrt(2f / bh)).toFloat
    private val SingleY = (sqrt(1f / bh) * sqrt(2f / bw)).toFloat

    //Don't actually store the matrix; select values on the fly
    def apply(x: Int, y: Int) = if(x == 0 && y == 0)
      Double
    else if(x == 0 && y != 0)
      SingleX
    else if(y == 0 && x != 0)
      SingleY
    else
      Sans
  }

  def apply(data: Matrix[Float]): Matrix[Float] = {
    val w = data.width
    val h = data.height
    val result = new MutableArrayMatrix[Float](w, h)
    val pi = 3.141592653589793238463f
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
            sum += data(x, y) * (cos(pi * u * (2.0 * x + 1.0) / (w * 2.0)) * cos(pi * v * (2.0 * y + 1.0) / (h * 2.0))).toFloat
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

    private val Double = (sqrt(1f / bw) * sqrt(1f / bh)).toFloat
    private val Sans = (sqrt(2f / bw) * sqrt(2f / bh )).toFloat
    private val SingleX = (sqrt(1f / bw) * sqrt(2f / bh)).toFloat
    private val SingleY = (sqrt(1f / bh) * sqrt(2f / bw)).toFloat

    //Don't actually store the matrix; select values on the fly
    def apply(x: Int, y: Int) = if(x == 0 && y == 0)
      Double
    else if(x == 0 && y != 0)
      SingleX
    else if(y == 0 && x != 0)
      SingleY
    else
      Sans
  }

  def apply(data: Matrix[Float]): Matrix[Float] = {
    val w = data.width
    val h = data.height
    val result = new MutableArrayMatrix[Float](w, h)
    val pi = 3.141592653589793238463f
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
            sum += (coeff(x, y) * data(x, y) * (cos(pi * x * (2.0 * u + 1.0) / (w * 2.0)) * cos(pi * y * (2.0 * v + 1.0) / (h * 2.0)))).toFloat
            x += 1
          }
          y += 1
        }
        result(u, v) = sum.round
        u += 1
      }
      v += 1
    }
    result
  }

}
