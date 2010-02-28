package org.purview.core.stage

import org.purview.core.analysis.Analyser
import scala.collection.mutable

object Stage {
  implicit def fromFunction[A, B](func: A => B) = new Stage[A, B] {
    val f = func
  }

  def apply[A, B](name: String)(func: A => B) = new Stage[A, B] {
    override val names = List(name)
    val f = func
  }
  
  def accumulation[A, B, C](first: Stage[A, B], second: Stage[B, C]) = new Stage[A, C] {
    val f = first.apply _ andThen second.apply _
    override val names = first.names ::: second.names
  }
  
  def identity[@specialized A] = new Stage[A, A] {
    val f = (x: A) => x
  }
}

object stage {
  def apply[A, B](name: String)(func: A => B) = new Stage[A, B] {
    override val names = List(name)
    val f = func
  }
}

trait Stage[@specialized -A, @specialized +B] extends Function1[A, B] {
  protected val f: Function1[A, B]

  val names: List[String] = Nil

  //A weak hash map will delete its values if available memory is low
  private val cache = new mutable.WeakHashMap[Any, Any]

  def apply(in: A): B = (cache.get(in) getOrElse {
    names match {
      case List(name) => Analyser.statistics.reportStage(name)
      case _ => //We are a higher-level stage or don't have a name
    }
    val out = f(in)
    cache(in) = out
    out
  }).asInstanceOf[B]

  def andThen[C](that: Stage[B, C]): Stage[A, C] =
    Stage.accumulation[A, B, C](this, that)
  def compose[C](that: Stage[C, A]): Stage[C, B] =
    Stage.accumulation[C, A, B](that, this)
}
