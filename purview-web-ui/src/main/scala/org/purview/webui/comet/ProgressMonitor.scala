package org.purview.webui.comet

import net.liftweb.http.CometActor
import net.liftweb.http.S
import net.liftweb.http.js.JsCmds._
import net.liftweb.util.Helpers._
import org.purview.webui.snippet.AnalysisSession
import org.purview.webui.util.AnalysisActor
import scala.xml.Text

class ProgressMonitor extends CometActor {
  val id = randomString(16)
  val contextPath = S.hostAndPath

  private var lastProgress = 0
  private var lastSubProgress = 0
  private var lastStatus = ""
  private var lastAnalyser = ""

  AnalysisSession.stats.is.foreach(_ ! AnalysisActor.AddListener(this))

  def fillPercent(percent: Int) =
    <div style={"width: " + percent + "%"} class={"ui-progressbar-value ui-widget-header " +
                                                  (if(percent > 99) "ui-corner-all" else "ui-corner-left")}></div>

  def mainBar(percent: Int) = <div id={id + "-main"} class="ui-progressbar ui-widget ui-widget-content ui-corner-all">{fillPercent(percent)}</div>

  def subBar(percent: Int) = <div id={id + "-sub"} class="ui-progressbar ui-widget ui-widget-content ui-corner-all">{fillPercent(percent)}</div>


  def render =
    <head><link href="http://ajax.googleapis.com/ajax/libs/jqueryui/1.8/themes/base/jquery-ui.css" rel="stylesheet" type="text/css"/>
<script src="http://ajax.googleapis.com/ajax/libs/jqueryui/1.8/jquery-ui.min.js"></script></head> ++
    bind("progress", defaultXml,
         "mainBar" -> mainBar(lastProgress),
         "subBar" -> subBar(lastSubProgress),
         "status" -> <div id={id + "-status"}>{lastStatus}</div>,
         "analyser" -> <div id={id + "-analyser"}>{lastAnalyser}</div>)

  override def lowPriority = {
    case AnalysisActor.ProgressUpdate(progress) =>
      lastProgress = (progress * 100).toInt
      partialUpdate(SetHtml(id + "-main", fillPercent(lastProgress)))
    case AnalysisActor.SubProgressUpdate(progress) =>
      lastSubProgress = (progress * 100).toInt
      partialUpdate(SetHtml(id + "-sub", fillPercent(lastSubProgress)))
    case AnalysisActor.StatusUpdate(status) =>
      lastStatus = status
      partialUpdate(SetHtml(id + "-status", Text(status)))
    case AnalysisActor.AnalyserUpdate(analyser) =>
      lastAnalyser = analyser
      partialUpdate(SetHtml(id + "-analyser", Text(analyser)))
    case AnalysisActor.Done =>
      lastStatus = "Done"
      partialUpdate(SetHtml(id + "-status", Text("Done")))
      lastAnalyser = "None"
      partialUpdate(SetHtml(id + "-analyser", Text("None")))
      partialUpdate(RedirectTo(contextPath + "/results"))
    case AnalysisActor.Error(error) =>
      this.error(error)
  }

}
