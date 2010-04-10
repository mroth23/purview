package org.purview.webui.util

import net.liftweb.actor.LiftActor
import net.liftweb.common.Logger
import org.purview.core.session.AnalysisStats
import scala.actors.Actor

object AnalysisActor {
  case class AddListener(listener: LiftActor)
  case class RemoveListener(listener: LiftActor)
  case class ProgressUpdate(progress: Float)
  case class SubProgressUpdate(progress: Float)
  case class AnalyserUpdate(analyser: String)
  case class StatusUpdate(status: String)
  case class Error(error: String)
  object Done
}

class AnalysisActor extends AnalysisStats with Actor with Logger {
  private var listeners: List[LiftActor] = Nil
  private var lastProgress = 0f
  private var lastSubProgress = 0f
  private var lastStatus = ""
  private var lastAnalyser = ""

  private val ProgressRoughness = 100

  def act = Actor.loop {
    Actor.react {
      case AnalysisActor.AddListener(listener) =>
        if(!listeners.contains(listener))
          listeners ::= listener
        listener ! AnalysisActor.ProgressUpdate(lastProgress)
        listener ! AnalysisActor.SubProgressUpdate(lastSubProgress)
        listener ! AnalysisActor.StatusUpdate(lastStatus)
        listener ! AnalysisActor.AnalyserUpdate(lastAnalyser)
      case AnalysisActor.RemoveListener(listener) =>
        listeners = listeners.filterNot (_ == listener)
    }
  }

  override def reportProgress(progress: Float) = if((progress * ProgressRoughness).toInt != (lastProgress * ProgressRoughness).toInt) {
    listeners.foreach(_ ! AnalysisActor.ProgressUpdate(progress))
    lastProgress = progress
  }

  override def reportSubProgress(progress: Float) = if((progress * ProgressRoughness).toInt != (lastSubProgress * ProgressRoughness).toInt) {
    listeners.foreach(_ ! AnalysisActor.SubProgressUpdate(progress))
    lastSubProgress = progress
  }

  override def reportAnalyser(analyser: String) = if(analyser != lastAnalyser) {
    listeners.foreach(_ ! AnalysisActor.AnalyserUpdate(analyser))
    lastAnalyser = analyser
  }

  override def reportStatus(status: String) = if(status != lastStatus) {
    listeners.foreach(_ ! AnalysisActor.StatusUpdate(status))
    lastStatus = status
  }

  def done() =
    listeners.foreach(_ ! AnalysisActor.Done)

  def error(error: String) = listeners.foreach(_ ! AnalysisActor.Error(error))
}
