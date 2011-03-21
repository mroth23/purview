package org.purview.snranalyser


import org.purview.core.analysis._
import org.purview.core.analysis.settings._
import org.purview.core.data._
import org.purview.core.report._
import org.purview.core.transforms._
import scala.math._
import scala.util.Sorting
class AnalyserImplementation extends Analyser[ImageMatrix] with Settings {

  val name = "Local CFA analyser"
  val description = "Analyses the CFA pattern of the image"

  override val version = Some("1.0")
  override val author = Some("Moritz Roth & David Flemström")

  val blockSizeSetting = IntRangeSetting("Block size", 8, 64)
  blockSizeSetting.value = 32


  val settings = List(blockSizeSetting)


  val extractGreen = for(matrix <- input) yield {
    for((x, y, color) <- matrix.cells) yield {
      color.g * 255f
    }
  }

  val highpassFilter = new ImmutableMatrix[Float](3, 3, Array(0, 1, 0, 1, -4, 1, 0, 1, 0))
  val filtered =  extractGreen >- Convolve(highpassFilter)

  val FragmentSize = blockSizeSetting.value

  val fragments = filtered >- Fragmentize(FragmentSize, FragmentSize)

  val res = for(matrix <- fragments) yield {

    for(cell <- matrix) yield {

      def getDiagonalVariances(w: Int, h: Int) : Array[Float] = {
        (for{
            x: Int <- 0 until w + h - 1
          } yield {

            val diag = (for{
                n: Int <- 0 until h
                if x - n >= 0 && x - n < w
              } yield {
                cell(x - n, n)
              })
            //as described in the paper, we don't calculate the actual variance here, but the mean of the pixels in the diagonal
            (diag.sum / diag.length).toFloat
          }).toArray
      }

      def getDFTMagnitudes(diag: Seq[Float]) : Array[Float] = {
        val dft = new JTransforms1D(diag.length)
        var data = new Array[Float](diag.length * 2)
        for(i: Int <- 0 until diag.length){
          data(i * 2) = diag(i)
          data(i * 2 + 1) = 0f
        }
        var transformed = dft.FFT1DForward(data)
        var result = new Array[Float](diag.length)
        for(i: Int <- 0 until diag.length){
          result(i) = sqrt(transformed(i * 2) * transformed(i * 2) + transformed(i * 2 + 1) * transformed(i * 2 + 1)).toFloat
        }
        result
      }

      val magnitudes = getDFTMagnitudes(getDiagonalVariances(cell.width, cell.height))
      val sorted = magnitudes.clone
      Sorting.quickSort(sorted)

      val magMedian = sorted((sorted.length / 2).round)

      val result = magnitudes.tail.map(x => x / magMedian) //.tail excludes the DC value

      val max = result.max
      val middle = (result.length / 2).round
      val peak = Array(result(middle - 1), result(middle), result(middle + 1)).max / max
      peak
    }
  }

  val image = for(in <- res) yield {
    for (x <- in) yield {
      Color(1.0f, x, x, x)
    }
  }

  def imageReport(img: Matrix[Color]): Set[ReportEntry] =
    Set(new ReportImage(Information, "Output image", 0, 0, img))

  val result = image >- imageReport

}