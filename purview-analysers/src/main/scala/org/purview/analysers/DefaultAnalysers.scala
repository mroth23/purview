package org.purview.analysers

import org.purview.core.analysis.Analyser
import org.purview.core.data.ImageMatrix

object DefaultAnalysers {
    List(() => new Bilinear, () => new ExampleAnalyser, () => new ErrorLevelAnalyser, () => new LuminanceGradient)
  val analysers: Seq[() => Analyser[ImageMatrix]] =
}
