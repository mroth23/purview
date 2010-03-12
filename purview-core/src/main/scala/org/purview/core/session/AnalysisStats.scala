package org.purview.core.session

class AnalysisStats {
  def reportProgress(progress: Float) = {}
  def reportSubProgress(progress: Float) = {}
  def reportStage(stage: String) = {}
  def reportAnalyser(analyser: String) = {}
  def reportStatus(status: String) = {}
}

case class SpartanAnalysisStats(progress: Float => Unit,
                                subProgress: Float => Unit,
                                stage: String => Unit,
                                analyser: String => Unit,
                                status: String => Unit) extends AnalysisStats {

  override def reportProgress(progress: Float) = this.progress(progress)
  override def reportSubProgress(progress: Float) = this.subProgress(progress)
  override def reportStage(stage: String) = this.stage(stage)
  override def reportAnalyser(analyser: String) =  this.analyser(analyser)
  override def reportStatus(status: String) = this.status(status)
}