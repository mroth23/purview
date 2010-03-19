package org.purview.core.process

object Computation {
  def apply[A](v: => A): Computation[A] = new Computation[A] {
    private[core] def value() = v
  }
  @inline def unit[A](v: => A) = apply(v)

  private[core] def get[A](s: Computation[A]) = s.value()
}

trait Computation[@specialized("Int,Float,Boolean") A] {
  private[core] def value(): A

  def map[B](f: A => B): Computation[B] = new Computation[B] {
    private[core] def value() = f(Computation.this.value())
  }
  
  @inline final def >-[B](f: A => B) = map(f)

  def flatMap[B](f: A => Computation[B]): Computation[B] = f(value())

  @inline final def >>=[B](f: A => Computation[B]) = flatMap(f)
}
