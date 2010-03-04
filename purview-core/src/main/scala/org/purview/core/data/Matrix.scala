package org.purview.core.data

import scala.collection.mutable.GenericArray

object Matrix {
  implicit def iterable[A](m: Matrix[A]) = new Iterable[A] {
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

trait Matrix[@specialized("Int,Float") +A] {
  val width: Int
  val height: Int
  def apply(x: Int, y: Int): A

  def map[B](f: A => B): Matrix[B] = {
    val result = map(f).toSeq
    new ImmutableMatrix(width, height, result)
  }

  def cells: Matrix[(Int, Int, A)] = {
    val result = new Array[(Int, Int, A)](width * height)

    var y = 0
    while(y < height) {
      var x = 0
      while(x < width) {
        result(x + y * width) = ((x, y, apply(x, y)))
        x += 1
      }
      y += 1
    }

    new ImmutableMatrix(width, height, result)
  }

  def filter(f: A => Boolean): Matrix[A] =
    if(this.forall(f)) this else error("A matrix cannot become filtered!")
}

sealed case class ImmutableMatrix[@specialized("Int,Float") +A](width: Int,
                                     height: Int,
                                     private val data: Seq[A]) extends Matrix[A] {
  def apply(x: Int, y: Int) = data(x + y * width)
}

sealed case class MutableMatrix[@specialized("Int,Float") A : Manifest]
    (width: Int, height: Int) extends Matrix[A] {
  private val data = new Array[A](width * height)
  def apply(x: Int, y: Int) = data(x + y * width)
  def update(x: Int, y: Int, value: A) = data(x + y * width) = value
}

sealed case class MutableColorMatrix(width: Int, height: Int) extends Matrix[Color] {
  private val buffer = new Array[Int](width * height)
  def apply(x: Int, y: Int) = Color fromRGB buffer(x + y * width)
  def update(x: Int, y: Int, value: Color) = (buffer(x + y * width) = value.toRGB)
}