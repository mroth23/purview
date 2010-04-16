package org.purview.metadataanalyser

import org.purview.core.analysis.Analyser
import org.purview.core.data.Computation
import org.purview.core.data.ImageMatrix
import org.purview.core.report.Error
import org.purview.core.report.ReportEntry
import org.purview.core.report.ReportMessage
import org.purview.core.report.Warning

class AnalyserImplementation extends Analyser[ImageMatrix] {
  val name = "Metadata analyser"
  val description = "Detects suspicious metadata entries in the file of an image"
  override val version = Some("1.0")
  override val author = Some("Moritz Roth & David Flemström")

  type MetaAnalyser = PartialFunction[(String, String, String), (Boolean, String)]

  private val metadataTesters: Set[MetaAnalyser] =
    Set(photoshopDetector, paintDotNetDetector, gimpDetector)

  val result: Computation[Set[ReportEntry]] = for(image <- input) yield {
    (for {
        tester <- metadataTesters
        tupled = tester.lift
        (dir, tree) <- image.metadata
        (key, value) <- tree
        (critical, msg) <- tupled((dir, key, value))
      } yield new ReportMessage(if(critical) Error else Warning, msg)).toSet
  }

  def photoshopDetector: MetaAnalyser = {
    case ("Exif", "Software", software) if software contains "Photoshop" =>
      (true, "The image was edited by Adobe® Photoshop")
    case ("Xmp", "history:saved", software) if (software contains "Photoshop") =>
      (true, "The image was once saved by Adobe® Photoshop")
    case ("Xmp", "history:created", software) if (software contains "Photoshop") =>
      (true, "The image was created by Adobe® Photoshop")
  }

  def paintDotNetDetector: MetaAnalyser = {
    case ("Exif", "Software", software) if software.toLowerCase contains "paint.net" =>
      (true, "The image was edited by Paint.NET")
  }

  def gimpDetector: MetaAnalyser = {
    case ("Exif", "Software", software) if software.toLowerCase contains "gimp" =>
      (true, "The image was edited by GIMP")
  }
}
