package org.purview.webui.util

import java.util.Date
import net.liftweb.actor.LiftActor
import net.liftweb.common.Full
import net.liftweb.widgets.flot.FlotAxisOptions
import net.liftweb.widgets.flot.FlotInfo
import net.liftweb.widgets.flot.FlotLegendOptions
import net.liftweb.widgets.flot.FlotLinesOptions
import net.liftweb.widgets.flot.FlotNewData
import net.liftweb.widgets.flot.FlotOptions
import net.liftweb.widgets.flot.FlotSerie
import scala.actors.Actor

object SystemSensor extends java.lang.Runnable {
  case class Update(time: Long, data: List[Double])
  
  @volatile private var initialized = false
  def start() = synchronized {
    if(!initialized) {
      initialized = true
      DataAccumulator.start()
      new Thread(this).start()
    }
  }

  override def run(): Unit = while(true) {
    val time = new Date().getTime

    val runtime = Runtime.getRuntime
    val newMem = (runtime.totalMemory - runtime.freeMemory).toDouble
    val newAlloc = runtime.totalMemory.toDouble

    DataAccumulator ! Update(time, List(newMem, newAlloc))

    Thread.sleep(1000)
  }
}

object DataAccumulator extends Actor {
  case class AddListener(actor: LiftActor)
  case class RemoveListener(actor: LiftActor)
  case class InitialData(data: FlotInfo)
  case class NewData(data: FlotNewData)
  val MaxData = 100

  val options = new FlotOptions {
    override val xaxis = Full(new FlotAxisOptions {
        override val mode = Full("time")
      })

    override val yaxis = Full(new FlotAxisOptions {
        override val min = Full(0.0)
      })

    override val legend = Full(new FlotLegendOptions {
        override val position = Full("nw")
        override val margin = Full(5)
      })
  }

  private var series: List[FlotSerie] = new FlotSerie {
    override val label = Full("Memory usage + cache")
    override val data = {
      val time = new Date().getTime
      (for(time <- (time - MaxData * 1000) to time by 1000l) yield (time.toDouble, 0.0)).toList
    }

    override val lines = Full (new FlotLinesOptions () {
        override val show = Full(true)
      })
  } :: new FlotSerie {
    override val label = Full("Available memory")
    override val data = {
      val time = new Date().getTime
      (for(time <- (time - MaxData * 1000) to time by 1000l) yield (time.toDouble, 0.0)).toList
    }

    override val lines = Full (new FlotLinesOptions () {
        override val show = Full(true)
      })
  } :: Nil

  private var listeners = List[LiftActor]()

  def act() = Actor.loop {
    Actor.react {
      case AddListener(l) =>
        listeners ::= l
        reply(InitialData(FlotInfo("", series, options)))
      case RemoveListener(l) =>
        listeners = listeners.filterNot(_ == l)
      case SystemSensor.Update(t, data) =>
        val time = t.toDouble

        val newSeries = (series zip data).map(d =>
          new FlotSerie() {
            override val label = d._1.label
            override val data = d._1.data.takeRight(MaxData - 1) ::: List((time, d._2))
          })

        series = newSeries
        val newData = data.map((time, _))
        listeners.foreach(_ ! NewData(FlotNewData(series, newData)))
    }
  }
}