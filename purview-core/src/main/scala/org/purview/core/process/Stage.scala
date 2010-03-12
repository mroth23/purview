package org.purview.core.process

import org.purview.core.analysis.Analyser

object Stage {
  def apply[A](name: String)(c: Computation[A]): Stage[A] = {
    val n = name
    //The "name" parameter is shadowed, and I don't want to
    //rename it because of named params

    new Stage[A] {
      val name = n
      val computation = c
    }
  }
}

trait Stage[@specialized("Int,Float,Boolean") A] extends Computation[A] {
  val name: String
  private[core] val computation: Computation[A]
  
  private[core] def value() = {
    val stats = Analyser.statistics
    stats.reportStage(name)
    stats.reportSubProgress(0)
    val result = computation.value()
    stats.reportProgress(1)
    result
  }
}
