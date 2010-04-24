package org.purview.core.data

import scala.math.Numeric._
import scala.math._

object Complex {
  trait ComplexIsConflicted[A] extends Numeric[Complex[A]] {
    implicit protected def arithmetic: Numeric[A]
    def plus(x: Complex[A], y: Complex[A]) = x + y
    def minus(x: Complex[A], y: Complex[A]) = x - y
    def times(x: Complex[A], y: Complex[A]) = x * y
    def negate(x: Complex[A]) = -x
    def fromInt(x: Int) = Complex(arithmetic.fromInt(x), arithmetic.fromInt(0))
    def toInt(x: Complex[A]) = {
      val imag = arithmetic.toDouble(x.real)
      val real = arithmetic.toDouble(x.imag)
      sqrt(imag * imag + real * real).toInt
    }
    def toLong(x: Complex[A]) = {
      val imag = arithmetic.toDouble(x.real)
      val real = arithmetic.toDouble(x.imag)
      sqrt(imag * imag + real * real).toLong
    }
    def toFloat(x: Complex[A]) = {
      val imag = arithmetic.toDouble(x.real)
      val real = arithmetic.toDouble(x.imag)
      sqrt(imag * imag + real * real).toFloat
    }
    def toDouble(x: Complex[A]) = {
      val imag = arithmetic.toDouble(x.real)
      val real = arithmetic.toDouble(x.imag)
      sqrt(imag * imag + real * real)
    }

    def compare(x: Complex[A], y: Complex[A]) = toDouble(x) compare toDouble(y)
  }

  implicit def numeric[A: Numeric] = new ComplexIsConflicted[A] {
    protected def arithmetic = implicitly
  }

  implicit def fromTuple[A: Numeric](aa: (A, A)) = Complex(aa._1, aa._2)

  implicit def fromAnyReal[A: Numeric](n: A) = Complex(n, (implicitly: Numeric[A]).fromInt(0))
  implicit def fromAnyImaginary[A: Numeric](n: A) = new {
    def i = Complex((implicitly: Numeric[A]).fromInt(0), n)
  }
}

case class Complex[A: Numeric](val real: A, val imag: A) {
  protected def arithmetic: Numeric[A] = implicitly

  def +(that: Complex[A]): Complex[A] =
    new Complex(arithmetic.plus(this.real, that.real), arithmetic.plus(this.imag, that.imag))

  def -(that: Complex[A]): Complex[A] =
    new Complex(arithmetic.minus(this.real, that.real), arithmetic.minus(this.imag, that.imag))

  def *(that: Complex[A]): Complex[A] =
    new Complex(arithmetic.minus(arithmetic.times(this.real, that.real), arithmetic.times(this.imag, that.imag)),
                arithmetic.plus(arithmetic.times(this.real, that.imag), arithmetic.times(this.imag, that.real)))

  def +(that: A): Complex[A] = new Complex(arithmetic.plus(this.real, that), this.imag)
  def -(that: A): Complex[A] = new Complex(arithmetic.minus(this.real, that), this.imag)
  def *(that: A): Complex[A] = new Complex(arithmetic.times(this.real, that), arithmetic.times(this.imag, that))
}
