package org.purview.core.process

import scala.collection.mutable.WeakHashMap

object Computation {
  private[core] case class Session()

  private[core] def apply[A](v: => A)(implicit s: Session = new Session): Computation[A] = new Computation[A] {
    def value = v
    def session = s
  }
  @inline private[core] def unit[A](v: => A)(implicit session: Session = new Session) = apply(v)

  private[core] def get[A](s: Computation[A]) = s.value
}

trait Computation[@specialized("Int,Float,Boolean") A] {
  protected def value: A
  protected[core] def session: Computation.Session

  private val sessionCache = new WeakHashMap[Computation.Session, A]
  private def sessionValue: A = sessionCache.getOrElse(session, {
      val v = value
      sessionCache(session) = v
      v
    })

  def map[B](f: A => B): Computation[B] = 
    new Computation[B] {
      def value: B = f(Computation.this.sessionValue)
      def session = Computation.this.session
    }
  
  @inline final def >-[B](f: A => B) = map(f)

  def flatMap[B](f: A => Computation[B]): Computation[B] = f(sessionValue)

  @inline final def >>=[B](f: A => Computation[B]) = flatMap(f)
}
