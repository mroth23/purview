package org.purview.webui.snippet

import net.liftweb.http.js.JsCmds
import net.liftweb.http.DispatchSnippet
import net.liftweb.http.SHtml
import scala.xml.NodeSeq

class System extends DispatchSnippet {
  def dispatch = {
    case "gc" => gc
    case "garbageCollect" => gc
  }

  def gc(in: NodeSeq) = SHtml.ajaxButton("Garbage collect", () => {System.gc(); JsCmds.Noop})
}
