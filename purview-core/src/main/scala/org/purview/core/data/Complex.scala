package org.purview.core.data

import scala.math._

class Complex(val real: Float, val imag: Float) {

  def +(c2: Complex): Complex = {
    new Complex(real + c2.real, imag + c2.imag)
  }

  def -(c2: Complex): Complex = {
    new Complex(real - c2.real, imag - c2.imag)
  }

  def *(c2: Complex): Complex = {
    new Complex(real * c2.real - imag * c2.imag,
                real * c2.imag + imag * c2.real)
  }

  def /(c2: Complex): Complex = {
    new Complex((real * c2.real + imag * c2.imag) / (c2.real * c2.real + c2.imag * c2.imag),
                (imag * c2.real - real * c2.imag) / (c2.real * c2.real + c2.imag * c2.imag))
  }

}
