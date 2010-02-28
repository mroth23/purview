package org.purview.core.session

import org.purview.core.analysis.Analyser
import org.purview.core.report.ReportEntry

class AnalysisSession[A](analysersToRun: Seq[Analyser[A]], input: A) {
  def run(stats: AnalysisStats): Map[Analyser[A], Set[ReportEntry]] = {
    val progScale = 1f / analysersToRun.length
    val results = for {
      i <- 0 until analysersToRun.length
      analyser = analysersToRun(i)
      progOffset = i * 1f / analysersToRun.length
      progReport = (x: Float) => stats.reportProgress(x * progScale + progOffset)
      localStats = new SpartanAnalysisStats(progReport, stats.reportStatus _,
                                            stats.reportStage _, stats.reportAnalyser _)
    } yield Analyser.analyseWithStats(analyser, input)(localStats)
    Map((analysersToRun zip results): _*)
  }
}
