package org.purview.core.session

class AnalysisStats {
  def reportProgress(progress: Float) = {}
  def reportSubProgress(progress: Float) = {}
  def reportAnalyser(analyser: String) = {}
  def reportStatus(status: String) = {}
}

case class SpartanAnalysisStats(progress: Float => Unit,
                                subProgress: Float => Unit,
                                analyser: String => Unit,
                                status: String => Unit) extends AnalysisStats {

  override def reportProgress(progress: Float) = this.progress(progress)
  override def reportSubProgress(progress: Float) = this.subProgress(progress)
  override def reportAnalyser(analyser: String) =  this.analyser(analyser)
  override def reportStatus(status: String) = this.status(status)
}
