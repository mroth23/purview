package org.purview.noopanalyser

import org.purview.core.analysis._
import org.purview.core.data._
import org.purview.core.process.Computation
import org.purview.core.report._
import org.purview.core.transforms._

class AnalyserImplementation extends Analyser[ImageMatrix] {
  val name = "No-Op Analyser"
  val description = "Returns the unmodified input image as its result"

  val result: Computation[Set[ReportEntry]] = input >- MatrixToImage() >-{ img => Set(new ReportEntry with Message with Image {
    val level = Information
    val message = "Input image"
    val x = 0
    val y = 0
    val image = img
  })}
}