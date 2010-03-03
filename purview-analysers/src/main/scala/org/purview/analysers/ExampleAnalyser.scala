package org.purview.analysers

import org.purview.core.analysis.Analyser
import org.purview.core.analysis.Metadata
import org.purview.core.data.Color
import org.purview.core.data.Matrix
import org.purview.core.report.Critical
import org.purview.core.report.Error
import org.purview.core.report.Information
import org.purview.core.report.ReportCircle
import org.purview.core.report.ReportCircleMove
import org.purview.core.report.ReportRectangle
import org.purview.core.report.ReportRectangleMove
import org.purview.core.report.Warning

class ExampleAnalyser extends Analyser[Matrix[Color]] with Metadata {
  val name = "Example analyser"
  val description = "Makes stuff up about what it finds"
  
  def analyse(in: Matrix[Color]) =
    Set(ReportRectangle(Information, "Wow, look at this!", 10, 10, 20, 20),
        ReportCircle(Warning, "This looks suspicious...", 50, 50, 40),
        ReportCircleMove(level = Error,
                         message = "This section was clearly moved!",
                         sourceX = 20,
                         sourceY = 50,
                         x = 80,
                         y = 80,
                         radius = 10),
        ReportRectangleMove(level = Critical,
                            message = "This was obviously moved",
                            sourceX = 100,
                            sourceY = 70,
                            x = 50,
                            y = 20,
                            width = 80,
                            height = 40))
}
