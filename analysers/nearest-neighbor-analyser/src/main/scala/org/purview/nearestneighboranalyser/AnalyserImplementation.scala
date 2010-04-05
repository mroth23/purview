package org.purview.nearestneighboranalyser

import org.purview.core.analysis.HeatMapImageAnalyser
import org.purview.core.data.Matrix
import org.purview.core.process.Computation
import org.purview.core.report.Warning
import scala.math._

class AnalyserImplementation extends HeatMapImageAnalyser {
  val name = "Nearest Neighbor"
  val description = "Finds nearest neighbor interpolated regions in an image"

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

      candidates contains src
    }
  }

  val findSquares: Computation[Matrix[Float]] = for (matrix <- markNearest) yield {
    status("Finding squares in uniform pixel regions")
    val countRange = 2 until 20

    for {
      (x,y,newSrc) <- matrix.cells
    } yield {
      if(newSrc) {
        countRange.findIndexOf { counter =>
          (x + counter < matrix.width - 1) && (y + counter < matrix.height - 1) &&
          matrix(x + counter, y) &&
          matrix(x, y + counter) &&
          matrix(x + counter, y + counter) && (for {
              dx <- 1 to counter
              dy <- 1 to counter
            } yield matrix(x + dx, y + dy)).count(x => x) < 3
        }.toFloat + 1f
      } else 0f
    }
  }

  private val gaussian30Kernel =
    (for(i <- -30 to 30) yield (30 - abs(i)) / (30f * 30f * 30f)).toArray

  override val convolve: Computation[Option[Array[Float]]] = Computation(Some(gaussian30Kernel))

  val heatmap = findSquares
}
