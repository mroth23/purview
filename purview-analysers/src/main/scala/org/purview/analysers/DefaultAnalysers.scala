package org.purview.analysers

import org.purview.core.analysis.Analyser
import org.purview.core.data.ImageMatrix

object DefaultAnalysers {
  val analysers: Seq[() => Analyser[ImageMatrix]] =
    List(() => new Bilinear, () => new ExampleAnalyser,
         () => new ErrorLevelAnalyser, () => new LuminanceGradient,
         () => new MetadataAnalyser)
}
