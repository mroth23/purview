package org.purview.luminancegradientanalyser

import java.awt.image.BufferedImage
import org.purview.core.analysis.Analyser
import org.purview.core.data.Color
import org.purview.core.data.ImageMatrix
import org.purview.core.report.Information
import org.purview.core.report.ReportEntry
import org.purview.core.report.ReportImage
import org.purview.core.transforms.Fragmentize
import org.purview.core.transforms.MatrixToImage
import scala.math._

class AnalyserImplementation extends Analyser[ImageMatrix] {
  val name = "Luminance gradient"
  val description = "Plots the general light direction in the image"
  override val version = Some("1.0")
  override val author = Some("Moritz Roth & David Flemström")

  val FragmentSize = 4

  val grayscale = {
    status("Converting image to grayscale")
    for(in <- input) yield
      in map (color => 0.2126f * color.r + 0.7152f * color.g + 0.0722f * color.b)
  }

  val fragments = grayscale >- Fragmentize(FragmentSize, FragmentSize)

  val vectors = for(frag <- fragments) yield {
    status("Calculating light direction vectors")
    for(cell <- frag) yield {
      val mid = cell(FragmentSize / 2, FragmentSize / 2)

      var maxX, maxY = 0
      var maxVal = 0f
      var minVal = 1f
      var y = 0
      while(y < FragmentSize) {
        var x = 0
        while(x < FragmentSize) {
          val value = cell(x, y)
          if(value < minVal)
            minVal = value
          if(value > maxVal) {
            maxX = x
            maxY = y
            maxVal = value
          }
          x += 1
        }
        y += 1
      }
      val len = sqrt((maxX - FragmentSize / 2) * (maxX - FragmentSize / 2) +
                     (maxY - FragmentSize / 2) * (maxY - FragmentSize / 2)).toFloat
      Color(1,
            maxX / FragmentSize,
            maxY / FragmentSize,
            (maxVal - minVal) * len)
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

  def imageReport(img: BufferedImage): Set[ReportEntry] =
    Set(new ReportImage(Information, "Output image", 0, 0, img))

  val result = luminanceGradient >- MatrixToImage() >- imageReport
}