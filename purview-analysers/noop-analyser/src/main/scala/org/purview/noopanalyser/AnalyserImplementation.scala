package org.purview.noopanalyser

import org.purview.core.analysis._
import org.purview.core.data._
import org.purview.core.process.Computation
import org.purview.core.report._
import org.purview.core.transforms._

class AnalyserImplementation extends Analyser[ImageMatrix] {
  val name = "No-Op Analyser"
  val description = "Returns the unmodified input image as its result"
  override val version = Some("1.0")
  override val author = Some("Moritz Roth & David Flemström")

  val result: Computation[Set[ReportEntry]] = input >- MatrixToImage() >-{ img =>
    Set(new ReportImage(Information, "Input image", 0, 0, img))
  }
}