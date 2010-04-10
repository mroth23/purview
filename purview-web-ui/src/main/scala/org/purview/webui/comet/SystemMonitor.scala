package org.purview.webui.comet

import net.liftweb.common.Full
import net.liftweb.http.CometActor
import net.liftweb.http.js.JsCmds
import net.liftweb.widgets.flot.Flot
import net.liftweb.widgets.flot.FlotInfo
import net.liftweb.widgets.flot.FlotOptions
import net.liftweb.widgets.flot.FlotSerie
import net.liftweb.widgets.flot.JsFlotAppendData
import org.purview.webui.util.DataAccumulator

class SystemMonitor extends CometActor {
  override def defaultPrefix = Full("graph")

  var options : FlotOptions = new FlotOptions {}
  var series : List[FlotSerie] = Nil

  def render = bind("monitor",
                    "graph" -> (Flot.render("graph", series, options, JsCmds.Noop)))

  override def localSetup {
    DataAccumulator !? DataAccumulator.AddListener(this) match {
      case DataAccumulator.InitialData(FlotInfo(_, s, o)) =>
        options = o
        series = s
    }
    super.localSetup
  }

  override def localShutdown {
    DataAccumulator ! DataAccumulator.RemoveListener(this)
    super.localShutdown
  }

  override def lowPriority : PartialFunction[Any, Unit] = {
    case DataAccumulator.NewData(d) =>
      series = d.series
      partialUpdate(JsFlotAppendData("graph", d.series, d.datas, true))
  }
}
