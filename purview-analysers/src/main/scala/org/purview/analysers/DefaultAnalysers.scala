package org.purview.analysers

import org.purview.core.analysis.Analyser
import org.purview.core.data.Color
import org.purview.core.data.Matrix

object DefaultAnalysers {
  val analysers: Seq[() => Analyser[Matrix[Color]]] =
    List(() => new Bilinear, () => new ExampleAnalyser, () => new ErrorLevelAnalyser, () => new LuminanceGradient)
}
