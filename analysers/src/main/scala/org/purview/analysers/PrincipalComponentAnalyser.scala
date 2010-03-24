package org.purview.analysers

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

class PrincipalComponentAnalyser extends Analyser[ImageMatrix] with Metadata with Settings {
  val name = "Principal Components Analysis"
  val description = "Plots each pixels distance to the 3 principal components"

  val scaleSetting = FloatRangeSetting("Scale of the garyscale pixel value", 0, 10)
  scaleSetting.value = 1 //default

  val settings = List(scaleSetting)

  private def scale = scaleSetting.value

  private case class ValueColorCell(value: Double, color: Color, x: Int, y: Int) extends Ordered[ValueColorCell] {
    def compare(that: ValueColorCell) = this.value.compare(that.value)
  }

  def DoPCA = for(matrix <- input) yield{
    status("Splitting the image into color channels")
    val width = matrix.width
    val height = matrix.height
    val red = new Array[Float](width * height)
    val green = new Array[Float](width * height)
    val blue = new Array[Float](width * height)

    var i = 0
    matrix.foreach { color =>
      red(i) = color.r
      green(i) = color.g
      blue(i) = color.b
      i += 1
    }

    var meanr = 0f
    var meang = 0f
    var meanb = 0f

    for(x <- 1 until red.length){
      meanr += red(x)
      meang += green(x)
      meanb += blue(x)
    }

    meanr /= red.length;
    meang /= green.length;
    meanb /= blue.length;

    for(x <- 1 until red.length){
      red(x) -= meanr
      green(x) -= meang
      blue(x) -= meanb
    }

    val covRR = covariance(red,red)
    val covRG = covariance(red,green)
    val covRB = covariance(red,blue)

    val covGR = covRG
    val covGG = covariance(green,green)
    val covGB = covariance(green,blue)

    val covBR = covRB
    val covBG = covGB
    val covBB = covariance(blue,blue)

    val covMatrix = new Array[Array[Float]](3)
    covMatrix(0) = Array[Float](covRR, covRG, covRB)
    covMatrix(1) = Array[Float](covGR, covGG, covGB)
    covMatrix(2) = Array[Float](covBR, covBG, covBB)



    status("Calculating covariance matrix")
  }

  //This returns the eigenvectors and their eigenvalues:
  //Every sub-array contains the eigenvector (index: 0,1,2) and the eigenvalue (index: 4)
  def getEigenvectors(matrix: Array[Array[Float]]) : Array[Array[Float]] = {
    //Preparation of the array
    val v = new Array[Array[Float]](3)
    v(0) = new Array[Float](3)
    v(1) = new Array[Float](3)
    v(2) = new Array[Float](3)

    val d = new Array[Float](3)
    val e = new Array[Float](3)

    for{
      x <- 0 until 3
      y <- 0 until 3
    }{
      v(x,y) = matrix(x,y)
    }

    //Tridiagonalize, long and epic step, this will fail :D
    for(j <- 0 until 3)
      d(j) = v(2, j)

    for{
      i <- 2 to 0 by -1
    }{
      var scale = 0f
      var h = 0f

      for(k <- 0 until i)
        scale += d(k).abs

      if (scale == 0.0f){
        e(i) = d(i - 1)

        for(j <- 0 until i){
          d(j) = v(i-1, j)
          v(i,j) = 0.0
          v(j,i) = 0.0
        }
      }
      else{
        for(k <- 0 until i){
          d(k) /= scale
          h += d(k) * d(k)
        }
        var f = d(i - 1)
        var g = sqrt(h)

        if(f > 0)
          g = (-g)
        e(i) = scale * g
        h = h - f * g
        d(i - 1) = f - g

        for(j <- 0 until i)
          e(j) = 0.0

        for(j <- 0 until i){
          f = d(j)
          v(j,i) = f
          g = e(j) + v(j,j) * f

          for(k <- j + 1 until i){
            g += v(k,j) * d(k)
            e(k) += v(k,j) * f
          }
          e(j) = g
        }
        //------------------------------
        f = 0.0
        for(j <- 0 until i){
          e(j) /= h
          f += e(j) * d(j)
        }

        var hh = f / (h + h)

        for(j <- 0 until i){
          e(j) -= hh * d(j)
        }

        for(j <- 0 until i){
          f = d(j)
          g = e(j)

          for(k <- j until i){
            v(k,j) -= (f * e(k) + g * d(k))
          }
          d(j) = v(i-1,j)
          v(i,j) = 0.0
        }
      }
      d(i) = h
    }

    //Accumulate transformations
    for (i <- 0 until 2){
      v(2,i) = v(i,i)
      v(i,i) = 1
      var h = d(i+1)
      if(h != 0.0){
        for(k <- 0 until i + 1)
          d(k) = v(k,i+1) / h
        for(j <- 0 until i + 1){
          var g = 0.0f
          for(k <- 0 until i + 1)
            g += v(k,i+1) * v(k,j)
          for(k <- 0 until i + 1)
            v(k,j) -= g * d(k)
        }
      }
      for(k <- 0 until i + 1)
        v(k,i+1) = 0.0f
    }
    for(j <- 0 until 3){
      d(j) = v(2,j)
      v(2,j) = 0.0f
    }
    v(2,2) = 1.0f
    e(0) = 0.0f

    //TODO: port tql2() here, nothing done yet
  }

  def covariance(a: Array[Float], b: Array[Float]) : Float = {
    var result = 0f

    for(x <- 1 until a.length)
      result += a(x) * b(x)

    result /= a.length
    result
  }

  def imageReport(): Set[ReportEntry] =
    Set(new ReportEntry with Image with Message {
        val x = 0
        val y = 0
        val image = null
        val message = "Output image"
        val level = Information
      })

  def result = DoPCA >- MatrixToImage() >- imageReport

}
