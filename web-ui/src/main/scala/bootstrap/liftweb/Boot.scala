package bootstrap.liftweb

import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.sitemap.Loc._
import net.liftweb.http.LiftRules
import net.liftweb.http.Req
import net.liftweb.sitemap.Loc
import net.liftweb.sitemap.Menu
import net.liftweb.sitemap.SiteMap
import org.purview.webui.snippet.AnalysisSession
import org.purview.webui.util.ImageManager
import org.purview.webui.util.ReportManager

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify Lift's environment
 */
class Boot {
  def boot {
    //Where to search for snippets
    LiftRules.addToPackages("org.purview.webui")

    //Build site map
    val entries =
      Menu(Loc("PurviewHome", List("index"), "Home")) ::
      Menu(Loc("PurviewImage", List("image"), "Choose image")) ::
      Menu(Loc("PurviewSession", List("analysers"), "Configure analysers",
               If(() => AnalysisSession.inputImage.is.isDefined, "You haven't selected an image yet"))) ::
      Menu(Loc("PurviewProcess", List("process"), "Process",
               If(() => AnalysisSession.running.is, "No running image session"))) ::
      Menu(Loc("PurviewResults", List("results"), "Results",
               If(() => ReportManager.reportExists(AnalysisSession.resultsKey.is), "No results to display"))) ::
      Menu(Loc("PurviewAbout", List("about"), "About Purview")) ::
      Nil
      
    LiftRules.setSiteMap(SiteMap(entries: _*))

    LiftRules.dispatch.append {
      case Req("imagefile" :: id :: Nil, _, _) =>
        () => ImageManager.serveImage(id)
    }
  }
}

