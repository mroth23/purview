package org.purview.nearestneighboranalyser

import org.purview.core.analysis.HeatMapImageAnalyser
import org.purview.core.data.Computation
import org.purview.core.data.Matrix
import org.purview.core.report.Warning
import scala.math._

class AnalyserImplementation extends HeatMapImageAnalyser {
  val name = "Nearest neighbor analyser"
  val description = "Finds nearest neighbor interpolated regions in an image"
  override val version = Some("2.6")
  override val author = Some("Moritz Roth & David Flemström")

  override val iconResource = Some("icons/analysers/nearest-neighbor.png")

  override val message = "Nearest neighbor scaled region"
  override val reportLevel = Warning

  val markNearest = for(matrix <- input) yield {
    status("Marking nearest neighbor scaled image regions")

    val width = matrix.width
    val height = matrix.height
    status("Finding uniform pixel regions")

    for {
      (x,y,src) <- matrix.cells
    } yield {
      val candidates = for {
        dx <- -1 to 1
        if dx != 0

        dy <- -1 to 1
        if dy != 0

        newx = x + dx
        if newx > -1 && newx < width

        newy = y + dy
        if newy > -1 && newy < height
      }yield matrix(newx, newy)

      if (candidates contains src) 1f else 0f
    }
  }

//  val findSquares: Computation[Matrix[Float]] = for (matrix <- markNearest) yield {
//    status("Finding squares in uniform pixel regions")
//    val countRange = 2 until 10
//
//    for {
//      (x,y,newSrc) <- matrix.cells
//    } yield {
//      if(newSrc) {
//        (for {
//          x0 <- countRange
//          y0 <- countRange
//        } yield {
//          (x + x0 < matrix.width - 1) && (y + y0 < matrix.height - 1) &&
//          matrix(x + x0, y) &&
//          matrix(x, y + y0) &&
//          matrix(x + x0, y + y0) && (for {
//              dx <- 1 to x0
//              dy <- 1 to y0
//            } yield matrix(x + dx, y + dy)).count(identity) < 3
//        }).lastIndexWhere(identity).toFloat
//      } else 0f
//    }
//  }

  val heatmap = markNearest
}
