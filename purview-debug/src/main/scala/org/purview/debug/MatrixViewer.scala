package org.purview.debug

import java.awt.Graphics2D
import java.awt.image.BufferedImage
import org.purview.core.data.Color
import org.purview.core.data.Matrix
import scala.swing.Component
import scala.swing.Frame
import scala.math._

class MatrixViewer[A](matrix: Matrix[A],
                      cellDrawer: (Int, Int, A, BufferedImage) => Unit) extends Frame {
  lazy val image = {
    val result = new BufferedImage(matrix.width, matrix.height, BufferedImage.TYPE_INT_ARGB)
    for((x, y, contents) <- matrix.cells) {
      cellDrawer(x, y, contents, result)
    }
    result
  }

  contents = new Component {
    override def paint(g: Graphics2D) = {
      g.drawImage(image, 0, 0, null)
    }
  }
}

object HeatmapViewer {
  def drawHeatmapCell(x: Int, y: Int, value: Float, img: BufferedImage) = {
    img.setRGB(x, y, 0xff000000 |
      min((value       * 255).toInt, 255) << 16 |
      min((value / 10  * 255).toInt, 255) << 8 |
      min((value / 100 * 255).toInt, 255))
  }
}

class HeatmapViewer(matrix: Matrix[Float]) extends MatrixViewer[Float](matrix, HeatmapViewer.drawHeatmapCell)

object ImageViewer {
  def drawHeatmapCell(x: Int, y: Int, value: Color, img: BufferedImage) = {
    img.setRGB(x, y,
      max(0, min((value.a * 255).toInt, 255)) << 24 |
      max(0, min((value.r * 255).toInt, 255)) << 16 |
      max(0, min((value.g * 255).toInt, 255)) << 8  |
      max(0, min((value.b * 255).toInt, 255)))
  }
}

class ImageViewer(matrix: Matrix[Color]) extends MatrixViewer[Color](matrix, ImageViewer.drawHeatmapCell)
