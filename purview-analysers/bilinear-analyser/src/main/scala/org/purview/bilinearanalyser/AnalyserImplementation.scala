package org.purview.bilinearanalyser

import org.purview.core.analysis._
import org.purview.core.analysis.settings._
import org.purview.core.data._
import org.purview.core.report.Warning
import scala.math._

class AnalyserImplementation extends HeatMapImageAnalyser with Settings {
  val name = "Bilinear analyser"
  val description = "Finds bilinearly interpolated regions in an image"
  override val version = Some("1.3")
  override val author = Some("Moritz Roth & David Flemström")

  override val iconResource = Some("icons/analysers/bilinear.png")

  val thresholdSetting = new FloatRangeSetting("Epsilon threshold", 0, 1, 100)
  thresholdSetting.value = 1f/255f

  val maxSizeFactorSetting = new IntRangeSetting("Max detected scale factor", 0, 16)
  maxSizeFactorSetting.value = 8

  override val message = "Bilinearly scaled region"
  override val reportLevel = Warning

  val settings = Seq(thresholdSetting, maxSizeFactorSetting)

  def thr = thresholdSetting.value
  def maxSizeFactor = maxSizeFactorSetting.value

  val markBilinear = for(matrix <- input) yield {
    status("Performing an amplitude scan")

    def mkSlopes(colors: List[Color], result: List[Color] = Nil): List[Color] = colors match {
      case x :: (rem @ (y :: _)) =>
        mkSlopes(rem, x - y :: result)
      case _ => result
    }

    def getExtent(base: Color, read: Int => Color, max: Int): Float = {
      val streak = for (extend <- 1 to max) yield read(extend)
      val slopes = mkSlopes(base :: streak.toList)
      if(slopes.nonEmpty) {
        val first = slopes.head

        if(abs(first.a) > thr ||
           abs(first.r) > thr ||
           abs(first.g) > thr ||
           abs(first.b) > thr) {
          val numberOfNearSlopes = slopes.findIndexOf {x =>
            !(abs(x.a - first.a) < thr &&
              abs(x.r - first.r) < thr &&
              abs(x.g - first.g) < thr &&
              abs(x.b - first.b) < thr)
          }
          math.max(0f, numberOfNearSlopes.toFloat / max)
        } else 0f
      } else 0f
    }

    for((x, y, color) <- matrix.cells) yield {
      val ri = getExtent(color, v => matrix(x + v, y), min(maxSizeFactor, matrix.width - x -1))
      val dn = getExtent(color, v => matrix(x, y + v), min(maxSizeFactor, matrix.height - y - 1))
      val dg = getExtent(color, v => matrix(x + v, y + v), min(maxSizeFactor, min(matrix.height - y - 1, matrix.width - x - 1)))
      ri * dn * dg
    }
  }

  val heatmap = markBilinear
}
