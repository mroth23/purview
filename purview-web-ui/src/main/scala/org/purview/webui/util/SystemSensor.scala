package org.purview.webui.util

import java.util.Calendar
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
    val time = Calendar.getInstance.getTimeInMillis

    val runtime = Runtime.getRuntime
    val newMem = (runtime.totalMemory - runtime.freeMemory).toDouble / (1024 * 1024)
    val newAlloc = runtime.totalMemory.toDouble / (1024 * 1024)

    DataAccumulator ! Update(time, List(newMem, newAlloc))

    Thread.sleep(1000)
  }
}

object DataAccumulator extends Actor {
  case class AddListener(actor: LiftActor)
  case class RemoveListener(actor: LiftActor)
  case class InitialData(data: FlotInfo)
  case class NewData(data: FlotNewData)
  val MaxData = 120 //Save 2 min history

  val options = new FlotOptions {
    override val xaxis = Full(new FlotAxisOptions {
        override val mode = Full("time")
        override val ticks = List(2.0) //One per minute
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
    override val label = Full("Memory usage &amp; cache (MiB)")
    override val data = {
      val time = Calendar.getInstance.getTimeInMillis
      (for(time <- (time - MaxData * 1000) to time by 1000l) yield (time.toDouble, 0.0)).toList
    }

    override val lines = Full (new FlotLinesOptions () {
        override val show = Full(true)
      })
  } :: new FlotSerie {
    override val label = Full("Available memory (MiB)")
    override val data = {
      val time = Calendar.getInstance.getTimeInMillis
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