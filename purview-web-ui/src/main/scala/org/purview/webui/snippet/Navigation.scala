package org.purview.webui.snippet

import net.liftweb.http.DispatchSnippet
import net.liftweb.http.S
import net.liftweb.http.SessionVar
import net.liftweb.util.Helpers._
import scala.xml.NodeSeq

object Navigation {
  object lastVisited extends SessionVar[List[(NodeSeq, String)]](Nil)

  val MaxHistory = 2
}

class Navigation extends DispatchSnippet {
  import Navigation._
  
  def dispatch = {
    case "breadcrumb" => breadcrumb
  }

  def breadcrumb(breadcrumbTemplate: NodeSeq): NodeSeq = {
    if(lastVisited.is.isEmpty || lastVisited.is.head._2 != S.uri)
      lastVisited.set((S.location.flatMap(_.linkText) openOr NodeSeq.Empty, S.uri) :: lastVisited.take(MaxHistory))

    def makeHistory(historyTemplate: NodeSeq): NodeSeq = lastVisited.is.drop(1).reverse.flatMap { entry =>
      bind("history", historyTemplate,
           "link" ->  <a href={entry._2}>{entry._1}</a>)
    }
    def makeCurrent(currentTemplate: NodeSeq) = lastVisited.is match {
      case last :: _ =>
        bind("current", currentTemplate,
             "link" -> <a href={last._2}>{last._1}</a>)
      case _ => NodeSeq.Empty
    }
    bind("breadcrumb", breadcrumbTemplate,
         "history" -> makeHistory _,
         "current" -> makeCurrent _)
  }
}
