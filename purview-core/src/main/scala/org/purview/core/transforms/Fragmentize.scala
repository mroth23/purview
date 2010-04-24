package org.purview.core.transforms

import org.purview.core.data.Matrix

case class Fragmentize[@specialized(Int, Float) A](blockWidth: Int, blockHeight: Int) extends Function1[Matrix[A], Matrix[Matrix[A]]] {

  private sealed class MatrixFragment(x: Int, y: Int, peer: Matrix[A]) extends Matrix[A] {
    val width = blockWidth
    val height = blockHeight
    def apply(x1: Int, y1: Int) = peer(x + x1, y + y1)
  }

  private sealed class FragmentedMatrix(peer: Matrix[A]) extends Matrix[Matrix[A]] {
    val width = peer.width - blockWidth
    val height = peer.height - blockHeight
    def apply(x: Int, y: Int) = {
      if(!(x < width))
        throw new ArrayIndexOutOfBoundsException("x")
      if(!(y < height))
        throw new ArrayIndexOutOfBoundsException("y")
      new MatrixFragment(x, y, peer)
    }
  }

  def apply(in: Matrix[A]): Matrix[Matrix[A]] = new FragmentedMatrix(in)
}
