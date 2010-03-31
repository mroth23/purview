package org.purview.core.session

import org.purview.core.analysis.Analyser
import org.purview.core.analysis.Metadata
import org.purview.core.report.ReportEntry

class AnalysisSession[A](analysersToRun: Seq[Analyser[A]], input: A) {
  def run(implicit stats: AnalysisStats): Map[Metadata, Set[ReportEntry]] = {
    val progScale = 1f / analysersToRun.length
    val results = for {
      i <- 0 until analysersToRun.length
      analyser = analysersToRun(i)
    } yield {
      stats.reportProgress(i * progScale)
      stats.reportSubProgress(0)
      analyser match {
        case m: Metadata =>
          stats.reportAnalyser(m.name)
        case _ => //No metadata available
          stats.reportAnalyser("Unknown")
      }
      val res = analyser.analyseWithStats(input)
      res
    }
    
    stats.reportProgress(1)
    stats.reportSubProgress(1)

    var i = 0

    Map((analysersToRun partialMap {
          case m: Metadata => m
          case analyser =>
            i += 1
            new Metadata {
              val name = "Unknown analyser " + i
              val description = "Unknown"
            }
        } zip results): _*)
  }
}
