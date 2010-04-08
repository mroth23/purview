package org.purview.luminancegradientanalyser

import java.awt.image.BufferedImage
import org.purview.core.analysis.Analyser
import org.purview.core.analysis.Settings
import org.purview.core.analysis.settings.FloatRangeSetting
import org.purview.core.data.Color
import org.purview.core.data.ImageMatrix
import org.purview.core.report.Information
import org.purview.core.report.Message
import org.purview.core.report.ReportEntry
import org.purview.core.report.ReportImage
import org.purview.core.transforms.MatrixToImage
import scala.math._

class AnalyserImplementation extends Analyser[ImageMatrix] {
  val name = "Luminance Gradient"
  val description = "Plots the general light direction in the image"

  val luminanceGradient = for(in <- input) yield {
    status("Calculating the luminance gradient for the image")
    for {
      (x, y, src) <- in.cells
    } yield if(x == 0 || x == in.width - 1 || y == 0 || y == in.height - 1)
      Color.Black
    else
    {
      val pixels = for {
        dx <- x - 1 to x + 1
        dy <- y - 1 to y + 1
        current = in(dx, dy)
        if !(x == 0 && y == 0)
      } yield (abs(dx-x) + abs(dy-y) match {
          case 1 => ValueColorCell(calculateBrightness(current) * 0.9712, current, dx, dy)
          case 2 => ValueColorCell(calculateBrightness(current) * 0.5445, current, dx, dy)
          case _ => ValueColorCell(0d, current, dx, dy)
        })

      val max = pixels.max
	val min = pixels.min

      val diff = 0.2 + sqrt(pow(min.color.r - max.color.r, 2) +
                      pow(min.color.g - max.color.g, 2) +
                      pow(min.color.b - max.color.b, 2))

      val gRes = max.x - min.x match {
	  case -2 => 1.00f
        case -1 => 0.75f
        case 0  => 0.50f
        case 1  => 0.25f
	  case 2  => 0.00f
      }

      val rRes = max.y - min.y match {
	  case -2 => 1.00f
        case -1 => 0.75f
        case 0  => 0.50f
        case 1  => 0.25f
	  case 2  => 0.00f
      }

      new Color(1, rRes, gRes, diff.toFloat)
    }
  }

  def imageReport(img: BufferedImage): Set[ReportEntry] =
    Set(new ReportImage(Information, "Output image", 0, 0, img))

  def calculateBrightness(pixel: Color) : Float = {
    //sRGB luminance
    (0.2126f * pixel.r + 0.7152f * pixel.g + 0.0722f * pixel.b)
  }

  val result = luminanceGradient >- MatrixToImage() >- imageReport
}

sealed case class ValueColorCell(value: Double, color: Color, x: Int, y: Int) extends Ordered[ValueColorCell] {
  def compare(that: ValueColorCell) = this.value.compare(that.value)
}
