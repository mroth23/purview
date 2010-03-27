package org.purview.analysers

import org.purview.core.analysis.Analyser
import org.purview.core.data.ImageMatrix

@deprecated
object DefaultAnalysers {
  val analysers: Seq[() => Analyser[ImageMatrix]] =
    List(() => new Bilinear,
         () => new ErrorLevelAnalyser, () => new LuminanceGradientAnalyser,
         () => new MetadataAnalyser, () => new CopyMoveAnalyser, () => new NearestNeighborAnalyser, () => new NoopAnalyser)
}
