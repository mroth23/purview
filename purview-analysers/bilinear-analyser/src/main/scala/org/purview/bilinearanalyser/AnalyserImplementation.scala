package org.purview.bilinearanalyser

import org.purview.core.analysis.HeatMapImageAnalyser
import org.purview.core.data._
import org.purview.core.report.Warning
import scala.math._

class AnalyserImplementation extends HeatMapImageAnalyser {
  val name = "Bilinear analyser"
  val description = "Finds bilinearly interpolated regions in an image"
  override val version = Some("1.3")
  override val author = Some("Moritz Roth & David Flemström")

  override val iconResource = Some("icons/analysers/bilinear.png")

  override val message = "Bilinearly scaled region"
  override val reportLevel = Warning

  val markHorizBilinear = for(matrix <- input) yield {
    status("Performing a vertical amplitude scan")

    @inline def cmp(color1: Color, color2: Color, e: Float) =
      abs(color1.a - color2.a) < e && abs(color1.r - color2.r) < e &&
      abs(color1.g - color2.g) < e && abs(color1.b - color2.b) < e

    val slopify: Seq[Color] => Color = x => x(0) - x(1)

    for((x, y, color) <- matrix.cells) yield {
      val horiz = (0 to 16).map(_ + x).takeWhile(matrix.width  >)
      val vert  = (0 to 16).map(_ + y).takeWhile(matrix.height >)

      if(horiz.length > 2 && vert.length > 2) {
        val streakRight = horiz            map (matrix(_, y))
        val streakDown  = vert             map (matrix(x, _))
        val streakDiag  = (horiz zip vert) map (t => matrix(t._1, t._2))

        val slopesRight = streakRight sliding 2 map slopify toSeq
        val slopesDown  = streakDown  sliding 2 map slopify toSeq
        val slopesDiag  = streakDiag  sliding 2 map slopify toSeq

        val firstRight  = slopesRight.head
        val firstDown   = slopesDown .head
        val firstDiag   = slopesDiag .head

        val rightLen = slopesRight.findIndexOf(x => cmp(firstRight, x, 1/255f))
        val downLen  = slopesDown .findIndexOf(x => cmp(firstDown,  x, 1/255f))
        val diagLen  = slopesDiag .findIndexOf(x => cmp(firstDiag,  x, 1/255f))

        rightLen + downLen + diagLen
      } else 0f
    }
  }

  val markVertBilinear = for(matrix <- input) yield {
    status("Performing a vertical amplitude scan")
    for((x, y, color) <- matrix.cells) yield
      if(y < matrix.height - 1)
        matrix(x, y + 1).weight - color.weight
    else
      0f
  }

  private val gaussian5BlurKernel = Array[Float](0.0080f, 0.016f, 0.024f, 0.032f, 0.04f, 0.032f,  0.024f, 0.016f, 0.0080f)

  override val convolve: Computation[Option[Array[Float]]] = Computation(Some(gaussian5BlurKernel))

  val heatmap = markHorizBilinear
}
