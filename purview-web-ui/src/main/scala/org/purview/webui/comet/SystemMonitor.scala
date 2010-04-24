package org.purview.webui.comet

import net.liftweb.common.Full
import net.liftweb.http.CometActor
import net.liftweb.http.S
import net.liftweb.http.js.JsCmds
import net.liftweb.util.Helpers
import net.liftweb.widgets.flot.Flot
import net.liftweb.widgets.flot.FlotInfo
import net.liftweb.widgets.flot.FlotOptions
import net.liftweb.widgets.flot.FlotSerie
import net.liftweb.widgets.flot.JsFlotAppendData
import org.purview.webui.util.DataAccumulator
import scala.xml.MetaData
import scala.xml.Null
import scala.xml.PrefixedAttribute
import scala.xml.UnprefixedAttribute

class SystemMonitor extends CometActor {
  override def defaultPrefix = Full("graph")
  val id = Helpers.nextFuncName
  val attrs = S.attrs.filter {
    case (Left(x), _) if x == "name" => false
    case (Left(x), _) if x == "type" => false
    case (Left(x), _) if x == "with" => false
    case (Left(x), _) if x == "at" => false
    case _ => true
  }

  var options : FlotOptions = new FlotOptions {}
  var series : List[FlotSerie] = Nil

  def attrsToMetadata(attrs: List[(Either[String, (String, String)], String)]) =
    attrs.foldLeft[MetaData](Null) {
      case (md, (Left(name), value)) => new UnprefixedAttribute(name, value, md)
      case (md, (Right((prefix, name)), value)) => new PrefixedAttribute(prefix, name, value, md)
      case _ => Null
    }

  def render = {
    <div id={id}></div> % attrsToMetadata(attrs) ++
    Flot.render(id, series, options, JsCmds.Noop)
  }

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
      partialUpdate(JsFlotAppendData(id, d.series, d.datas, true))
  }
}
