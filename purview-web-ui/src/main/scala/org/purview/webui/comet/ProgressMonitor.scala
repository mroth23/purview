package org.purview.webui.comet

import net.liftweb.common.Full
import net.liftweb.http.CometActor
import net.liftweb.http.S
import net.liftweb.http.js.JsCmds._
import net.liftweb.util.Helpers
import net.liftweb.util.Helpers._
import org.purview.webui.snippet.AnalysisSession
import org.purview.webui.util.AnalysisActor
import org.purview.webui.db.Database
import org.squeryl.PrimitiveTypeMode._
import scala.xml.Text

class ProgressMonitor extends CometActor {
  val id = nextFuncName
  val contextPath = S.hostAndPath

  override def defaultPrefix = Full("progress")

  private var lastProgress = 0
  private var lastSubProgress = 0
  private var lastStatus = ""
  private var lastAnalyser = ""

  override def localSetup() =
    AnalysisSession.runningAnalyses.get(name flatMap(x => Helpers.tryo(transaction {
            from(Database.analyses)(analysis => where(analysis.handle === x) select(analysis.id)).single
          })) openOr -1l).foreach(_ ! AnalysisActor.AddListener(this))

  override def localShutdown() =
    AnalysisSession.runningAnalyses.get(name flatMap(x => Helpers.tryo(transaction {
            from(Database.analyses)(analysis => where(analysis.handle === x) select(analysis.id)).single
          })) openOr -1l).foreach(_ ! AnalysisActor.RemoveListener(this))

  def fillPercent(percent: Int) =
    <div style={"width: " + percent + "%"} class={"ui-progressbar-value ui-widget-header " +
                                                  (if(percent > 99) "ui-corner-all" else "ui-corner-left")}></div>

  def mainBar(percent: Int) = <div id={id + "-main"} class="ui-progressbar ui-widget ui-widget-content ui-corner-all">{fillPercent(percent)}</div>

  def subBar(percent: Int) = <div id={id + "-sub"} class="ui-progressbar ui-widget ui-widget-content ui-corner-all">{fillPercent(percent)}</div>


  def render = bind("progress", defaultXml,
                    "mainBar" -> mainBar(lastProgress),
                    "subBar" -> subBar(lastSubProgress),
                    "status" -> <div id={id + "-status"}>{lastStatus}</div>,
                    "analyser" -> <div id={id + "-analyser"}>{lastAnalyser}</div>)

  override def mediumPriority = {
    case AnalysisActor.StatusUpdate(status) =>
      lastStatus = status
      partialUpdate(SetHtml(id + "-status", Text(status)))
    case AnalysisActor.AnalyserUpdate(analyser) =>
      lastAnalyser = analyser
      partialUpdate(SetHtml(id + "-analyser", Text(analyser)))
  }

  override def highPriority = {
    case AnalysisActor.ProgressUpdate(progress) =>
      lastProgress = (progress * 100).toInt
      partialUpdate(SetHtml(id + "-main", fillPercent(lastProgress)))
    case AnalysisActor.SubProgressUpdate(progress) =>
      lastSubProgress = (progress * 100).toInt
      partialUpdate(SetHtml(id + "-sub", fillPercent(lastSubProgress)))
    case AnalysisActor.Done =>
      lastStatus = "Done"
      partialUpdate(SetHtml(id + "-status", Text("Done")))
      lastAnalyser = "None"
      partialUpdate(SetHtml(id + "-analyser", Text("None")))
      partialUpdate(RedirectTo(contextPath + "/analysis/" + (name openOr "unknown") + "/results"))
    case AnalysisActor.Error(error) =>
      this.error(error)
  }

}
