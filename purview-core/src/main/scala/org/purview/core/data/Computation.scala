package org.purview.core.data

import scala.collection.mutable.WeakHashMap

object Computation {
  private[core] class Session {
    override def equals(x: Any) = x match {
      case that: AnyRef => this eq that
    }
  }

  def apply[A](v: => A): Computation[A] = new Computation[A] {
    def calculateValue(session: Computation.Session) = v
  }

  def unit[A](v: () => A): Computation[A] = new Computation[A] {
    def calculateValue(session: Computation.Session) = v()
  }

  private[core] def get[A](s: Computation[A])(implicit session: Session) = s.value(session)
}

/**
 * A monad that accumulates computations in a tree-like fashion.
 * By using Computation.get(), it is possible to calculate a session-dependent
 * value of a node in the computation tree.
 */
trait Computation[@specialized(Int, Float, Boolean) A] {
  protected def calculateValue(session: Computation.Session): A

  private var maxRefCount = 1
  private var refCounts: Map[Computation.Session, Int] = Map.empty.withDefault(_ => maxRefCount)
  private val sessionCache = new WeakHashMap[Computation.Session, A]

  private[core] final def value(implicit session: Computation.Session): A = {
    val result = sessionCache.getOrElse(session, {
      val v = calculateValue(session)
      sessionCache(session) = v

      refCounts = refCounts.updated(session, refCounts(session) - 1)
      if(refCounts(session) == 0) {
        sessionCache.clear()
        System.gc()
      }
      assert(refCounts(session) >= 0, error("Reference leak"))
      
      v
    })
  
    result
  }

  def map[B](f: A => B): Computation[B] = {
    maxRefCount += 1
    new Computation[B] {
      def calculateValue(session: Computation.Session): B = f(Computation.this.value(session))
    }
  }

  final def >-[B](f: A => B) = map(f)

  def flatMap[B](f: A => Computation[B]): Computation[B] = {
    maxRefCount += 1
    new Computation[B] {
      def calculateValue(session: Computation.Session): B = f(Computation.this.value(session)).value(session)
    }
  }

  final def >>=[B](f: A => Computation[B]) = flatMap(f)
}
