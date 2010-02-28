package org.purview.core.session

class AnalysisStats {
  def reportProgress(progress: Float) = {}
  def reportStatus(status: String) = {}
  def reportStage(stage: String) = {}
  def reportAnalyser(analyser: String) = {}
}

class SpartanAnalysisStats(progress: Float => Unit,
                           status: String => Unit,
                           stage: String => Unit,
                           analyser: String => Unit) extends AnalysisStats {
  override def reportProgress(progress: Float) = this.progress(progress)
  override def reportStatus(status: String) = this.status(status)
  override def reportStage(stage: String) = this.stage(stage)
  override def reportAnalyser(analyser: String) =  this.analyser(analyser)
}