package org.purview.core.data

import org.purview.core.analysis.Analyser
import scala.collection.mutable.GenericArray

object Matrix {
  /** Adds support for everything that Iterable supports to Matrix */
  implicit def iterable[@specialized("Int,Float,Boolean") A](m: Matrix[A]) = new Iterable[A] {
    def iterator = new Iterator[A] {
      private var y = 0
      private var x = 0
      def hasNext = x + y * m.width + 1 < m.width * m.height
      def next = {
        x += 1
        if(x == m.width) {
          x = 0
          y += 1
        }
        m.apply(x, y)
      }
    }
  }
}

trait Matrix[@specialized("Int,Float,Boolean") +A] extends NotNull {
  val width: Int
  val height: Int
  def apply(x: Int, y: Int): A

  def map[B : Manifest](f: A => B): Matrix[B] = {
    val result: MutableMatrix[B] = if(implicitly[Manifest[B]] <:< implicitly[Manifest[Color]])
      (new MutableColorMatrix(width, height)).asInstanceOf[MutableMatrix[B]]
    else
      new MutableArrayMatrix[B](width, height)

    var y = 0
    while(y < height) {
      Analyser.statistics.reportSubProgress(y.toFloat / height)
      var x = 0
      while(x < width) {
        result(x, y) = f(apply(x, y))
        x += 1
      }
      y += 1
    }
    
    result
  }

  def zip[@specialized("Int,Float,Boolean") B](that: Matrix[B]): Matrix[(A, B)] = {
    val data = new GenericArray[(A, B)](width * height)
    require(this.width == that.width, "Matrices must have the same width")
    require(this.height == that.height, "Matrices must have the same height")
    
    var y = 0
    while(y < height) {
      Analyser.statistics.reportSubProgress(y.toFloat / height)
      var x = 0
      while(x < width) {
        data(x + y * width) = (this.apply(x, y), that.apply(x, y))
        x += 1
      }
      y += 1
    }
    
    new ImmutableMatrix(width, height, data)
  }

  def foreach(f: A => Unit) = {
    var y = 0
    while(y < height) {
      Analyser.statistics.reportSubProgress(y.toFloat / height)
      var x = 0
      while(x < width) {
        f(apply(x, y))
        x += 1
      }
      y += 1
    }
  }

  def cells: Matrix[(Int, Int, A)] = {
    val data = new Array[(Int, Int, A)](width * height)

    var y = 0
    while(y < height) {
      var x = 0
      while(x < width) {
        data(x + y * width) = ((x, y, apply(x, y)))
        x += 1
      }
      y += 1
    }

    new ImmutableMatrix(width, height, data)
  }

  def filter(f: A => Boolean): Matrix[A] =
    if(this.forall(f)) this else error("A matrix cannot become filtered!")
}

sealed case class ImmutableMatrix[@specialized("Int,Float,Boolean") +A](width: Int, height: Int, private val data: Seq[A]) extends Matrix[A] {
  def apply(x: Int, y: Int) = data(x + y * width)
}

trait MutableMatrix[@specialized("Int,Float,Boolean") A] extends Matrix[A] {
  def update(x: Int, y: Int, value: A)
}

sealed case class MutableArrayMatrix[@specialized("Int,Float,Boolean") A : Manifest](width: Int, height: Int) extends MutableMatrix[A] {
  private val data = new Array[A](width * height)
  def apply(x: Int, y: Int) = data(x + y * width)
  def update(x: Int, y: Int, value: A) = data(x + y * width) = value
}

sealed case class MutableColorMatrix(width: Int, height: Int) extends MutableMatrix[Color] {
  private val buffer = new Array[Int](width * height)
  def apply(x: Int, y: Int) = Color fromRGB buffer(x + y * width)
  def update(x: Int, y: Int, value: Color) = (buffer(x + y * width) = value.toRGB)
}
