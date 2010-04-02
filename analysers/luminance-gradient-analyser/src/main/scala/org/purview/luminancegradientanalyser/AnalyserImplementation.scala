package org.purview.luminancegradientanalyser

import java.awt.image.BufferedImage
import org.purview.core.analysis.Analyser
import org.purview.core.analysis.Metadata
import org.purview.core.analysis.Settings
import org.purview.core.analysis.settings.FloatRangeSetting
import org.purview.core.data.Color
import org.purview.core.data.ImageMatrix
import org.purview.core.report.Image
import org.purview.core.report.Information
import org.purview.core.report.Message
import org.purview.core.report.Point
import org.purview.core.report.ReportEntry
import org.purview.core.transforms.MatrixToImage
import scala.math._

class AnalyserImplementation extends Analyser[ImageMatrix] with Metadata with Settings {
  val name = "Luminance Gradient"
  val description = "Plots the general light direction in the image"

  val scaleSetting = FloatRangeSetting("Blue scale", 2, 20)
  scaleSetting.value = 10 //default

  val blendSetting = FloatRangeSetting("Blending", 0f, 1f)
  blendSetting.value = 0f

  val settings = List(scaleSetting, blendSetting)

  private def scale = scaleSetting.value
  private def blending = blendSetting.value

  val luminanceGradient = for(in <- input) yield {
    status("Calculating the luminance gradient for the image")
    for {
      (x, y, src) <- in.cells
      srcBrightness = sqrt(pow(src.r, 2) *  0.241 + pow(src.g, 2) *  0.691 + pow(src.b, 2) *  0.068)
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
          case 1 => ValueColorCell(sqrt(pow(current.r, 2) *  0.241 +
                                        pow(current.g, 2) *  0.691 +
                                        pow(current.b, 2) *  0.068) * 0.9712, current, dx, dy)
          case 2 => ValueColorCell(sqrt(pow(current.r, 2) *  0.241 +
                                        pow(current.g, 2) *  0.691 +
                                        pow(current.b, 2) *  0.068) * 0.5445, current, dx, dy)
          case _ => ValueColorCell(0d, current, dx, dy)
        })

      val max = pixels.max
      val diff = sqrt(pow(src.r - max.color.r, 2) +
                      pow(src.g - max.color.g, 2) +
                      pow(src.b - max.color.b, 2))

      val clr1 = x - max.x match {
        case -1 => new Color(1, 0, 0, 0)
        case 0 => new Color(1, 0, 0.5f, 0)
        case 1 => new Color(1, 0, 1, 0f)
      }

      val clr2 = y - max.y match {
        case -1 => new Color(1f, 0f, clr1.g, 0f)
        case 0 => new Color(1f, 0.5f, clr1.g, 0f)
        case 1 => new Color(1f, 1f, clr1.g, 0f)
      }

      new Color(1,
                (1 - blending) * clr2.r               + blending * src.r,
                (1 - blending) * clr2.g               + blending * src.g,
                (1 - blending) * diff.toFloat * scale + blending * src.b)
    }
  }

  def imageReport(img: BufferedImage): Set[ReportEntry] =
    Set(new ReportEntry with Image with Message {
        val x = 0
        val y = 0
        val image = img
        val message = "Output image"
        val level = Information
      })

  val result = luminanceGradient >- MatrixToImage() >- imageReport

}

sealed case class ValueColorCell(value: Double, color: Color, x: Int, y: Int) extends Ordered[ValueColorCell] {
  def compare(that: ValueColorCell) = this.value.compare(that.value)
}
