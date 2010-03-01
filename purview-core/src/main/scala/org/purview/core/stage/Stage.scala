package org.purview.core.stage

import org.purview.core.analysis.Analyser
import org.purview.core.session.SpartanAnalysisStats
import scala.collection.mutable

object Stage {
  implicit def fromFunction[@specialized("Int,Float") A,
                            @specialized("Int,Float") B](func: A => B) = new Stage[A, B] {
    val name = "Anonymous function-based stage"
    val f = func
  }

  def apply[@specialized("Int,Float") A,
            @specialized("Int,Float") B](nam: String)(func: A => B) = new Stage[A, B] {
    val name = nam
    val f = func
  }

  def accumulation[@specialized("Int,Float") A,
                   @specialized("Int,Float") B,
                   @specialized("Int,Float") C]
      (first: Stage[A, B], second: Stage[B, C]) = new Stage[A, C] {
    val f = (in: A) => {

      val oldStats = Analyser.statistics
      val stats1 = new SpartanAnalysisStats((progress: Float) => {
        oldStats.reportProgress(progress * 0.5f)
      }, oldStats.reportStatus, oldStats.reportStage, oldStats.reportAnalyser)

      val result = Analyser.statsVar.withValue(stats1) (first.apply(in))

      val stats2 = new SpartanAnalysisStats((progress: Float) => {
        oldStats.reportProgress(0.5f + progress * 0.5f)
      }, oldStats.reportStatus, oldStats.reportStage, oldStats.reportAnalyser)

      Analyser.statsVar.withValue(stats2) (second.apply(result))
    }
    override protected[stage] def names = first.names ::: second.names
    val name = "A combination of \"" + names.mkString("\", \"") + "\""
  }

  def identity[@specialized("Int,Float") A] = new Stage[A, A] {
    val name = "The identity stage"
    val f = (x: A) => x
  }
}

object stage {
  def apply[@specialized("Int,Float") A,
            @specialized("Int,Float") B](nam: String)(func: A => B) = new Stage[A, B] {
    val name = nam
    val f = func
  }
}

//TODO: add caching to stages! (Doesn't work at the moment because of A's contravariance)
trait Stage[@specialized("Int,Float") -A, @specialized("Int,Float") +B] {
  protected val f: Function1[A, B]

  val name: String

  protected[stage] def names: List[String] = List(name)

  def apply(in: A): B = {
      if(names.length == 1) {
        Analyser.statistics.reportStage(name)
        Analyser.statistics.reportStatus("→ Running step \"" + name + "\"")
      }

      val out = f(in)

      if(names.length == 1) {
        Analyser.statistics.reportStatus("→ Done running step \"" + name + "\"")
      }
      out
    }

  def andThen[@specialized("Int,Float") C](that: Stage[B, C]): Stage[A, C] =
    Stage.accumulation[A, B, C](this, that)
  def compose[@specialized("Int,Float") C](that: Stage[C, A]): Stage[C, B] =
    Stage.accumulation[C, A, B](that, this)

  override def toString = name
}
