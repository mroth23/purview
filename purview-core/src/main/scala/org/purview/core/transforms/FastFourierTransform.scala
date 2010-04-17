package org.purview.core.transforms

import org.purview.core.data._
import scala.math._

class FastFourierTransform() {

  def FFT1D(data: Array[Complex]) : Array[Complex] = {
    var result = data

    val n = data.length

    if (n == 1){
      result = Array(data(0))
    }
    else{
      if (n % 2 == 0){
        var even = new Array[Complex](n / 2)
        var odd = new Array[Complex](n / 2)

        for{
          i : Int <- 0 until n / 2
        }{
          even(i) = data(2 * i)
          odd(i) = data(2 * i + 1)
        }

        val evenFFT = FFT1D(even)
        val oddFFT = FFT1D(odd)
        val pi = 3.14159265359f

        for{
          i: Int <- 0 until n / 2
        }{
          val a = -2 * i * pi / n
          val c = new Complex(cos(a).toFloat, sin(a).toFloat)
          result(i) = evenFFT(i) + c * oddFFT(i)
          result(i + n / 2) = evenFFT(i) - c * oddFFT(i)
        }
      }
      else{
        throw new IllegalArgumentException("Length of the FFT must be a power of 2")
      }
    }

    result
  }

  def FFT2D(data: Array[Array[Complex]]) = {
    var result = data
    var temp = new Array[Complex](data(0).length)

    for{ a : Int <- 0 until data.length }{
      for{ b : Int <- 0 until data(0).length }{
        temp(b) = result(a)(b)
      }
      temp = FFT1D(temp)
      for{ b : Int <- 0 until data(0).length }{
        result(a)(b) = temp(b)
      }
    }
    temp = new Array[Complex](data.length)
    for{ a : Int <- 0 until data.length }{
      for{ b : Int <- 0 until data(0).length }{
        temp(b) = result(b)(a)
      }
      temp = FFT1D(temp)
      for{ b : Int <- 0 until data(0).length }{
        result(b)(a) = temp(b)
      }
    }

    result
  }
  
}
