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
    status("Converting image to luminance values")
    for(in <- input) yield
      in map (color => 0.2126f * color.r + 0.7152f * color.g + 0.0722f * color.b)
  }

  val fragments = grayscale >- Fragmentize(FragmentSize, FragmentSize)

  val vectors = for(frag <- fragments) yield {
    status("Calculating light direction vectors")
    val pi = Pi.toFloat
    val off = -(FragmentSize - 1) / 2f
    for(cell <- frag) yield {
      //Compute gradients in x and y direction
      val vecX = Matrix.sequence(cell.cells).foldLeft(0f)((acc, next) => acc + next._3 * (next._1 + off))
      val vecY = Matrix.sequence(cell.cells).foldLeft(0f)((acc, next) => acc + next._3 * (next._2 + off))
      //Get gradient angle (angle from polar coordinates)
      val asimuth = if(vecX > 0f && vecY >= 0f)
        atan(vecY/vecX)
      else if(vecX > 0f && vecY < 0f)
        atan(vecY/vecX) + 2f * pi
      else if (vecX < 0f)
        atan(vecY/vecX) + pi
      else if (vecX == 0f && vecY > 0f)
        pi / 2f
      else if (vecX == 0f && vecY < 0f)
        (3f * pi) / 2f
      else //x == 0 && y == 0
        0f
      //Get the color corresponding to the angle
      Color(1f, -sin(asimuth).toFloat / 2f + 0.5f, -cos(asimuth).toFloat / 2f + 0.5f, 0f)
    }
  }

  val edge = for(frags <- fragments) yield {
    //Compute the gradient vector length, this is basically a sobel edge detector
    for(in <- frags) yield {
      val a11 = in(0, 0)
      val a12 = in(0, 1)
      val a13 = in(0, 2)
      val a21 = in(1, 0)
      val a22 = in(1, 1)
      val a23 = in(1, 2)
      val a31 = in(2, 0)
      val a32 = in(2, 1)
      val a33 = in(2, 2)
      sqrt {
        2 * {
          a11 * a11 +
          2 * a11 * {
            a12 + a21 -
            a23 - a32 - a33
          } +
          2 * a12 * a12 +
          2 * a12 * {
            a13 - a31 -
            a32 - a32 - a33
          } +
          a13 * a13 -
          2 * a13 * {
            a21 - a23 +
            a31 + a32
          } +
          2 * a21 * a21 -
          2 * a21 * {
            a23 + a23 -
            a31 + a33
          } +
          2 * a23 * a23 -
          2 * a23 * {
            a31 - a33
          } +
          a31 * a31 +
          2 * a31 * a32 +
          2 * a32 * a32 +
          2 * a32 * a33 +
          a33 * a33
        }
      }.toFloat
    }
  }

  val luminanceGradient = for(in <- input; vecs <- vectors; edg <- edge) yield {
    status("Merging the luminance gradient for the image")
    
    for((x, y, color) <- in.cells) yield {
      val xoff = x - FragmentSize / 2
      val yoff = y - FragmentSize / 2
      if(xoff > 0 && yoff > 0 && xoff < vecs.width && yoff < vecs.height) {
        val c = vecs(xoff, yoff)
        Color(1f, c.r, c.g, edg(xoff, yoff))
      } else Color.Black
    }
  }

  def imageReport(img: Matrix[Color]): Set[ReportEntry] =
    Set(new ReportImage(Information, "Output image", 0, 0, img))

  val result = luminanceGradient >- imageReport
}
