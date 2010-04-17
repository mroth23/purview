package org.purview.core.data

import java.io.Serializable
import org.purview.core.analysis.Analyser
import scala.collection.mutable.GenericArray

object Matrix {
  /** Adds support for everything that IndexedSeq supports to Matrix */
  implicit def sequence[A](m: Matrix[A]) = new IndexedSeq[A] {
    def length = m.width * m.height
    def apply(i: Int) = m.apply(i % m.width, i / m.width)
  }
}

trait Matrix[@specialized("Int, Float, Boolean") +A] extends Serializable with NotNull {
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

  def zip[@specialized("Int, Float, Boolean") B](that: Matrix[B]): Matrix[(A, B)] = {
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

  private sealed class LazyTransformedMatrix[@specialized("Int, Float, Boolean") +A,
                                             @specialized("Int, Float, Boolean") +B]
  (peer: Matrix[B], func: (Int, Int, B) => A) extends Matrix[A] {
    val width = peer.width
    val height = peer.height
    def apply(x: Int, y: Int) = func(x, y, peer(x, y))
  }

  def cells: Matrix[(Int, Int, A)] = new LazyTransformedMatrix(this, (_: Int, _: Int, _: A))

  def filter(f: A => Boolean): Matrix[A] =
    if(Matrix.sequence(this).forall(f)) this else error("A matrix cannot become filtered!")

  override def equals(other: Any) = other match {
    case that: Matrix[_] if this.width == that.width && this.height == that.height =>
      var result = true
      var x = 0
      while(x < this.width && result) {
        var y = 0
        while(y < this.height && result) {
          result = (this(x, y) == that(x, y))
          y += 1
        }
        x += 1
      }
      result
    case _ => false
  }

  override def hashCode = Matrix.sequence(this).hashCode
}

sealed case class ImmutableMatrix[@specialized("Int, Float, Boolean") +A](width: Int, height: Int, private val data: Seq[A]) extends Matrix[A] {
  def apply(x: Int, y: Int) = data(x + y * width)
}

trait MutableMatrix[@specialized("Int, Float, Boolean") A] extends Matrix[A] {
  def update(x: Int, y: Int, value: A)
}

sealed case class MutableArrayMatrix[@specialized("Int, Float, Boolean") A : Manifest](width: Int, height: Int) extends MutableMatrix[A] {
  private val data = new Array[A](width * height)
  def apply(x: Int, y: Int) = data(x + y * width)
  def update(x: Int, y: Int, value: A) = data(x + y * width) = value
}

sealed case class MutableColorMatrix(width: Int, height: Int) extends MutableMatrix[Color] {
  private val buffer = new Array[Float](width * height * 4)
  def apply(x: Int, y: Int) = {
    val i = (x + y * width) * 4
    Color(buffer(i), buffer(i + 1), buffer(i + 2), buffer(i + 3))
  }
  def update(x: Int, y: Int, value: Color) = {
    val i = (x + y * width) * 4
    buffer(i) = value.a
    buffer(i + 1) = value.r
    buffer(i + 2) = value.g
    buffer(i + 3) = value.b
  }
}
