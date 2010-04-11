package bootstrap.liftweb

import net.liftweb.util.Helpers._
import net.liftweb.sitemap.Loc._
import net.liftweb.common.Full
import net.liftweb.http.DocType
import net.liftweb.http.LiftRules
import net.liftweb.http.ParsePath
import net.liftweb.http.Req
import net.liftweb.http.ResponseInfo
import net.liftweb.http.RewriteRequest
import net.liftweb.http.RewriteResponse
import net.liftweb.http.S
import net.liftweb.sitemap.Loc
import net.liftweb.sitemap.Menu
import net.liftweb.sitemap.SiteMap
import net.liftweb.widgets.flot.Flot
import org.purview.webui.snippet.AnalysisSession
import org.purview.webui.util.ImageManager
import org.purview.webui.util.ReportManager
import org.purview.webui.util.SystemSensor

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify Lift's environment
 */
class Boot {
  def boot {
    //Where to search for snippets
    LiftRules.addToPackages("org.purview.webui")

    def analysisId = S.param("analysisId") openOr ""
    //Build site map
    val entries =
      Menu(Loc("PurviewHome", List("index"), "Purview Home")) ::
      Menu(Loc("PurviewImage", List("image"), "Analyse image")) ::
      Menu(Loc("PurviewSession", List("analysers"), "Configure analysers",
               If(() => AnalysisSession.analyses.is.contains(analysisId), "There's no active image"),
               Hidden)) ::
      Menu(Loc("PurviewProcess", List("process"), "Process",
               If(() => (AnalysisSession.analyses.is.get(analysisId).flatMap(_.runtime.map(_.running)) getOrElse false), "No running image session"),
               Hidden)) ::
      Menu(Loc("PurviewResults", List("results"), "Results",
               If(() => ReportManager.reportExists(AnalysisSession.analyses.is.get(analysisId).flatMap(_.runtime.map(_.resultsKey)) getOrElse ""), "No results to display"),
               Hidden)) ::
      Menu(Loc("PurviewAbout", List("about"), "About Purview")) ::
      Nil

    LiftRules.setSiteMap(SiteMap(entries: _*))

    LiftRules.statelessRewrite.append {
      case RewriteRequest(ParsePath(List("image", id, "analysers"), _, _, _), _, _) =>
        RewriteResponse("analysers" :: Nil, Map("analysisId" -> id))
      case RewriteRequest(ParsePath(List("image", id, "process"), _, _, _), _, _) =>
        RewriteResponse("process" :: Nil, Map("analysisId" -> id))
      case RewriteRequest(ParsePath(List("image", id, "results"), _, _, _), _, _) =>
        RewriteResponse("results" :: Nil, Map("analysisId" -> id))
    }

    LiftRules.dispatch.append {
      case Req("imagefile" :: id :: Nil, _, _) =>
        () => ImageManager.serveImage(id)
    }

    ResponseInfo.docType = {
      case _ if S.getDocType._1 => S.getDocType._2
      case _ => Full(DocType.xhtml11)
    }

    Flot.init()
    SystemSensor.start()
  }
}

