package org.purview.analysers

import org.purview.core.analysis.Analyser
import org.purview.core.analysis.Metadata
import org.purview.core.analysis.Settings
import org.purview.core.analysis.settings.IntRangeSetting
import org.purview.core.analysis.settings.FloatRangeSetting
import org.purview.core.data.Color
import org.purview.core.data.ImmutableMatrix
import org.purview.core.data.Matrix
import org.purview.core.data.MutableMatrix
import org.purview.core.report.Warning
import org.purview.core.report.Critical
import org.purview.core.report.Error
import org.purview.core.report.Information
import org.purview.core.report.ReportCircle
import org.purview.core.report.ReportCircleMove
import org.purview.core.report.ReportRectangle
import org.purview.core.report.ReportRectangleMove
import org.purview.core.data._
import org.purview.core.analysis._
import org.purview.core.report._
import org.purview.core.transforms._
import scala.math._
import java.awt.image.BufferedImage

class LuminanceGradient extends Analyser[Matrix[Color]] with Metadata with Settings {
  val name = "Luminance Gradient"
  val description = "Plots the general light direction in the image"

  val scaleSetting = FloatRangeSetting("Scale of the blue channel", 2, 20)
  scaleSetting.value = 10 //default

  val blendSetting = FloatRangeSetting("Percentage of the original pixel to be included in the output pixel", 0f, 1f)
  blendSetting.value = 0f

  val settings = List(scaleSetting, blendSetting)

  private def scale = scaleSetting.value
  private def blending = blendSetting.value

  def luminanceGradient = {
    for(in <- input) yield{
      val result = new MutableMatrix[Color](in.width, in.height)
      var dx1 = 0f
      var dy1 = 0f
      var clr = new Color(0,0,0,0)

      for {
        y <- 1 until in.height - 1
        x <- 1 until in.width - 1
        src = in(x, y)
        srcBrightness = sqrt(pow(src.r, 2) *  0.241 + pow(src.g, 2) *  0.691 + pow(src.b, 2) *  0.068)
      } {
        val pixels = for {
          dx <- x - 1 to x + 1
          dy <- y - 1 to y + 1
          current = in(dx, dy)
          if(!(x == 0 && y == 0))
            } yield (((dx-x).abs + (dy-y).abs) match {
                case 1 => (sqrt(pow(current.r, 2) *  0.241 + pow(current.g, 2) *  0.691 + pow(current.b, 2) *  0.068) * 0.9712, current.r, current.g, current.b, dx, dy)
                case 2 => (sqrt(pow(current.r, 2) *  0.241 + pow(current.g, 2) *  0.691 + pow(current.b, 2) *  0.068) * 0.5445, current.r, current.g, current.b, dx, dy)
                case _ => (0d, current.r, current.g, current.b, dx, dy)
              })

          val max = pixels.max
          val diff = sqrt(pow(src.r - max._2, 2) + pow(src.g - max._3, 2) + pow(src.b - max._4, 2)) / 255

          dx1 = x - max._5
          dy1 = y - max._6

          if (dx1 == -1){
            clr = new Color(1f, 0f, 0f, 0f)
          }
          else if (dx1 == 0){
            clr = new Color(1f, 0f, 0.5f, 0f)
          }
          else if (dx1 == 1){
            clr = new Color(1f, 0f, 1f, 0f)
          }

          var temp = clr

          if (dy1 == -1){
            clr = new Color(1f, 0f, temp.g, 0f)
          }
          else if (dy1 == 0){
            clr = new Color(1f, 0.5f, temp.g, 0f)
          }
          else if (dy1 == 1){
            clr = new Color(1f, 1f, temp.g, 0f)
          }

          temp = clr

          result(x,y) = new Color(1,
                                  (1 - blending) * temp.r                  + blending * src.r,
                                  (1 - blending) * temp.g                  + blending * src.g,
                                  (1 - blending) * diff.floatValue * scale + blending * src.b)
        }
        result
      }
    }

    def imageReport(img: BufferedImage): Set[ReportEntry] = Set(new ReportEntry with Point with Image with Message {
        val x = 0
        val y = 0
        val image = img
        val message = "Output image"
        val level = Information
      })

    def result = luminanceGradient >- MatrixToImage() >- imageReport

  }
