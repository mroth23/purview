package org.purview.webui.snippet

import net.liftweb.http.DispatchSnippet
import net.liftweb.http.S
import scala.xml.NodeSeq

class Process extends DispatchSnippet {
  def dispatch = {
    case "progress" => progress
  }

  def progress(template: NodeSeq) = <lift:comet type="ProgressMonitor" name={S.param("analysisId") openOr ""}>{template}</lift:comet>
}
