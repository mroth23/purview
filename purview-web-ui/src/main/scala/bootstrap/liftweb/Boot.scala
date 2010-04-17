package bootstrap.liftweb

import net.liftweb.util.Helpers._
import net.liftweb.sitemap.Loc._
import java.io.BufferedInputStream
import java.io.FileInputStream
import net.liftweb.common.Full
import net.liftweb.http.DocType
import net.liftweb.http.LiftRules
import net.liftweb.http.ParsePath
import net.liftweb.http.Req
import net.liftweb.http.ResponseInfo
import net.liftweb.http.RewriteRequest
import net.liftweb.http.RewriteResponse
import net.liftweb.http.S
import net.liftweb.http.StreamingResponse
import net.liftweb.sitemap.Loc
import net.liftweb.sitemap.Menu
import net.liftweb.sitemap.SiteMap
import net.liftweb.widgets.flot.Flot
import org.purview.webui.db.Controller
import org.purview.webui.util.ImageManager
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
      Menu(Loc("PurviewImage", List("new_analysis"), "Analyse image")) ::
      Menu(Loc("PurviewSession", List("configure_analysers"), "Configure analysers",
               If(() => true, "There's no active image"),
               Hidden)) ::
      Menu(Loc("PurviewProcess", List("progress"), "Process",
               If(() => true, "No running image session"),
               Hidden)) ::
      Menu(Loc("PurviewResults", List("results"), "Results",
               If(() => true, "No results to display"),
               Hidden)) ::
      Menu(Loc("PurviewAbout", List("about"), "About Purview")) ::
      Nil

    LiftRules.setSiteMap(SiteMap(entries: _*))

    LiftRules.statelessRewrite.append {
      case RewriteRequest(ParsePath(List("analysis", id, "configure_analysers"), _, _, _), _, _) =>
        RewriteResponse("configure_analysers" :: Nil, Map("analysisId" -> id))
      case RewriteRequest(ParsePath(List("analysis", id, "progress"), _, _, _), _, _) =>
        RewriteResponse("progress" :: Nil, Map("analysisId" -> id))
      case RewriteRequest(ParsePath(List("analysis", id, "results"), _, _, _), _, _) =>
        RewriteResponse("results" :: Nil, Map("analysisId" -> id))
    }

    LiftRules.dispatch.append {
      case Req("image" :: id :: Nil, "png", _) if ImageManager.exists(id) =>
        () => {
          val file = ImageManager.file(id)
          val stream = new BufferedInputStream(new FileInputStream(file))
          Full(new StreamingResponse(stream, stream.close, file.length, List("Content-Type" -> "image/png"), Nil, 200))
        }
    }

    ResponseInfo.docType = {
      case _ if S.getDocType._1 => S.getDocType._2
      case _ => Full(DocType.xhtml11)
    }

    Flot.init()
    SystemSensor.start()
    Controller.init()
  }
}

