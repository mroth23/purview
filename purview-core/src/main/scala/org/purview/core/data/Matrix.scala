package org.purview.core.data

import scala.collection.mutable.ArrayBuffer

trait Matrix[@specialized +A] extends NotNull {
  val width: Int
  val height: Int
  val data: Seq[A]

  @inline def apply(x: Int, y: Int): A = data(x + y * width)

  def map[@specialized B : Manifest](f: A => B): Matrix[B] = {
    val newData = new Array[B](width * height)
    var i = 0
    while(i < width * height) {
      newData(i) = f(data(i))
      i += 1
    }
    new ImmutableMatrix(width, height, newData)
  }

  def foreach(f: A => Unit): Unit = {
    var i = 0
    while(i < width * height) {
      f(data(i))
      i += 1
    }
  }
  
  def cells: Matrix[(Int, Int, A)] = {
    val result = new ArrayBuffer[(Int, Int, A)](width * height)

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

  def filter(f: A => Boolean): Matrix[A] = if(data exists f) error("A matrix cannot become filtered!") else this
}

sealed case class ImmutableMatrix[@specialized A](width: Int, height: Int, data: Seq[A]) extends Matrix[A]

sealed case class MutableMatrix[@specialized A : Manifest](width: Int, height: Int) extends Matrix[A] {
  private val buffer = new Array[A](width * height)
  val data: Seq[A] = buffer
  def update(x: Int, y: Int, value: A) = buffer(x + y * width) = value
}
