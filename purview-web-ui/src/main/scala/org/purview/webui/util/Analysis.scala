package org.purview.webui.util

import org.purview.core.analysis.Analyser
import org.purview.core.data.ImageMatrix

case class Analysis(inputImage: InputImage,
                    analysers: Option[Analysers],
                    runtime: Option[AnalysisRuntime])

case class InputImage(name: String,
                      original: ImageManager.Image,
                      scaled: ImageManager.Image)

case class Analysers(instances: Seq[Analyser[ImageMatrix]],
                     enabled: Map[Analyser[ImageMatrix], Boolean])

case class AnalysisRuntime(running: Boolean, resultsKey: String, stats: AnalysisActor)
