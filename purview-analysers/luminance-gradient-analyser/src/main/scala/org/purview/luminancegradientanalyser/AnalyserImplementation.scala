package org.purview.luminancegradientanalyser

import org.purview.core.analysis.Analyser
import org.purview.core.data.Color
import org.purview.core.data.ImageMatrix
import org.purview.core.data.Matrix
import org.purview.core.report.Information
import org.purview.core.report.ReportEntry
import org.purview.core.report.ReportImage
import org.purview.core.transforms.Fragmentize
import scala.math._

class AnalyserImplementation extends Analyser[ImageMatrix] {
  val name = "Luminance gradient"
  val description = "Plots the general light direction in the image"
  override val version = Some("1.0")
  override val author = Some("Moritz Roth & David Flemström")

  val FragmentSize = 3

  val grayscale = {
    status("Converting image to grayscale")
    for(in <- input) yield
      in map (color => 0.2126f * color.r + 0.7152f * color.g + 0.0722f * color.b)
  }

  val fragments = grayscale >- Fragmentize(FragmentSize, FragmentSize)

  val vectors = for(frag <- fragments) yield {
    status("Calculating light direction vectors")
    val pi = Pi.toFloat
    for(cell <- frag) yield {
      val off = -(FragmentSize - 1) / 2f
      val vecX = Matrix.sequence(cell.cells).foldLeft(0f)((acc, next) => acc + next._3 * (next._1 + off))
      val vecY = Matrix.sequence(cell.cells).foldLeft(0f)((acc, next) => acc + next._3 * (next._2 + off))
      val len = sqrt(vecX * vecX + vecY * vecY)
      val asimuth = if(vecY == 0f && vecX == 0f)
        0f
      else if(vecX >= 0f)
        asin(vecY / len)
      else
        -asin(vecY / len) + pi

      Color(1, -sin(asimuth).toFloat / 2 + 0.5f, -cos(asimuth).toFloat / 2 + 0.5f, 0)
    }
  }

  val luminanceGradient = for(in <- input; vecs <- vectors) yield {
    status("Merging the luminance gradient for the image")
    
    for((x, y, color) <- in.cells) yield {
      val xoff = x - FragmentSize / 2
      val yoff = y - FragmentSize / 2
      if(xoff > 0 && yoff > 0 && xoff < vecs.width && yoff < vecs.height)
        vecs(xoff, yoff)
      else
        Color.Black
    }
  }

  def imageReport(img: Matrix[Color]): Set[ReportEntry] =
    Set(new ReportImage(Information, "Output image", 0, 0, img))

  val result = luminanceGradient >- imageReport
}
