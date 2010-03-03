package org.purview.core.session

import org.purview.core.analysis.Analyser
import org.purview.core.analysis.Metadata
import org.purview.core.report.ReportEntry

class AnalysisSession[A](analysersToRun: Seq[Analyser[A]], input: A) {
  def run(stats: AnalysisStats): Map[Analyser[A], Set[ReportEntry]] = {
    val progScale = 1f / analysersToRun.length
    val results = for {
      i <- 0 until analysersToRun.length
      analyser = analysersToRun(i)
      progOffset = i * progScale
      progReport = (x: Float) => stats.reportProgress(x * progScale + progOffset)
      localStats = new SpartanAnalysisStats(progReport, stats.reportStatus _,
                                            stats.reportStage _, stats.reportAnalyser _)
    } yield {
      localStats.reportProgress(progOffset)
      analyser match {
        case m: Metadata =>
          localStats.reportAnalyser(m.name)
          localStats.reportStatus("⇒ Using analyser \"" + m.name + "\"" )
        case _ => //No metadata available
          localStats.reportAnalyser("Unknown")
          localStats.reportStatus("⇒ Using a nameless analyser" )
      }
      val res = analyser.analyseWithStats(input)(localStats)
      analyser match {
        case m: Metadata =>
          localStats.reportStatus("⇒ Done using analyser \"" + m.name + "\"" )
        case _ => //No metadata available
          localStats.reportStatus("⇒ Done using analyser" )
      }
      res
    }
    
    stats.reportProgress(1)
    stats.reportAnalyser("None")
    stats.reportStage("None")
    stats.reportStatus("Done")
    
    Map((analysersToRun zip results): _*)
  }
}
