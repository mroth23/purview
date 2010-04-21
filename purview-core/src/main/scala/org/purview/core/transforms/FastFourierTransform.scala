package org.purview.core.transforms

import org.purview.core.data._
import scala.math.Numeric.FloatIsFractional
import scala.math._

case class FastFourierTransform1D() extends Function1[Seq[Complex[Float]], Seq[Complex[Float]]] {
  def apply(data: Seq[Complex[Float]]): Seq[Complex[Float]] = {
    val n = data.length
    val nhalf = n / 2
    if(n == 1)
      data
    else if(n % 2 == 0) {
      val even = (0 until n) filter (_ % 2 == 0) map data
      val odd = (0 until n) filter (_ % 2 == 1) map data

      val evenFFT = apply(even)
      val oddFFT = apply(odd)
      val pi = 3.14159265359f

      (for {
        i <- 0 until nhalf
      } yield {
        val a = -2 * i * pi / n
        val c = Complex(cos(a).toFloat, sin(a).toFloat)
        evenFFT(i) + c * oddFFT(i)
      }) ++ (for {
        i <- nhalf until n
      } yield {
        val a = -2 * (i - nhalf) * pi / n
        val c = Complex(cos(a).toFloat, sin(a).toFloat)
        evenFFT(i) - c * oddFFT(i)
      })
    } else throw new IllegalArgumentException("Length of the FFT input must be a power of 2")
  }
}

case class FastFourierTransform2D() extends Function1[Matrix[Complex[Float]], Matrix[Complex[Float]]] {
  def apply(data: Matrix[Complex[Float]]): Matrix[Complex[Float]] = {
    val fft1d = FastFourierTransform1D()
    val result = new MutableArrayMatrix[Complex[Float]](data.width, data.height)
    var temp: Array[Complex[Float]] = new Array[Complex[Float]](data.height)

    for(a <- 0 until data.width) {
      for(b <- 0 until data.height)
        temp(b) = data(a, b)

      temp = fft1d(temp).toArray

      for(b <- 0 until data.height)
        result(a, b) = temp(b)
    }
    
    temp = new Array[Complex[Float]](data.width)
    for(a <- 0 until data.width) {
      for(b <- 0 until data.height)
        temp(b) = result(b, a)

      temp = fft1d(temp).toArray

      for(b <- 0 until data.height)
        result(b, a) = temp(b)
    }
    result
  }
}
