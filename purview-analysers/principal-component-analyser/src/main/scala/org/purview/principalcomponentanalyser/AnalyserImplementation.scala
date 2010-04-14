package org.purview.principalcomponentanalyser

import org.purview.core.report._
import org.purview.core.transforms._
import Jama.Matrix
import Jama.EigenvalueDecomposition
import org.purview.core.data.Matrix
import org.purview.core.data.Color
import java.awt.image.BufferedImage
import org.purview.core.analysis.Analyser
import org.purview.core.data.ImageMatrix
import org.purview.core.report.ReportEntry
import org.purview.core.report.ReportImage
import scala.math._

class AnalyserImplementation extends Analyser[ImageMatrix]{
  val name = "PCA"
  val description = "Performs principal component analysis on the image."

  def splitChannels(in: org.purview.core.data.Matrix[Color]): Array[Array[Double]] = {
    //Split the image into one jagged array of floats,
    //array(0) = red, array(1) = blue etc.
    //This is not implementet yet as you can see
    val rgb = new Array[Array[Double]](3)

    rgb(0) = new Array[Double](in.width * in.height)
    rgb(1) = new Array[Double](in.width * in.height)
    rgb(2) = new Array[Double](in.width * in.height)

    var counter = 0

    for{
      x : Int <- 0 until in.width
      y : Int <- 0 until in.height
    }{
      rgb(0)(counter) = in.apply(x,y).r
      rgb(1)(counter) = in.apply(x,y).g
      rgb(2)(counter) = in.apply(x,y).b
      counter += 1
    }
    
    rgb
  }

  def zeroMean(rgb: Array[Array[Double]]): Array[Array[Double]] = {
    //substract the mean from each element in the arrays
    var r = rgb(0)
    var g = rgb(1)
    var b = rgb(2)
    val meanr = r.sum.toDouble / r.length.toDouble
    val meang = g.sum.toDouble / g.length.toDouble
    val meanb = b.sum.toDouble / b.length.toDouble

    val result = new Array[Array[Double]](3)

    result(0) = r map (_ - meanr)
    result(1) = g map (_ - meang)
    result(2) = b map (_ - meanb)
    result
  }

  def covariance(a1: Array[Double], a2: Array[Double]): Double = {
    //Calculate the covariance between 2 arrays
    var result = 0.0d
    for{
      x <- 0 until a1.length
    }{
      result += (a1(x) * a2(x))
    }
    (result / a1.length).toFloat
  }

  def covarianceMatrix(rgb: Array[Array[Double]]): Array[Array[Double]] = {
    val r = rgb(0)
    val g = rgb(1)
    val b = rgb(2)

    val covRR = covariance(r, r);
    val covRG = covariance(r, g);
    val covRB = covariance(r, b);

    val covGR = covRG;
    val covGG = covariance(g, g);
    val covGB = covariance(g, b);

    val covBR = covRB;
    val covBG = covGB;
    val covBB = covariance(b, b);

    val covMatrix = new Array[Array[Double]](3)

    covMatrix(0) = Array(covRR, covRG, covRB)
    covMatrix(1) = Array(covGR, covGG, covGB)
    covMatrix(2) = Array(covBR, covBG, covBB)

    covMatrix
  }

  def getEigenvectors(in: Array[Array[Double]]): Array[Array[Double]] = {
    val matrix = new Jama.Matrix(in)
    val ed = new EigenvalueDecomposition(matrix)
    
    ed.getV.getArray()
  }

  def DotP(vector1: Array[Double], vector2: Array[Double]): Double = {
    (vector1(0) * vector2(0) + vector1(1) * vector2(1) + vector1(2) * vector2(2)).toDouble
  }

  def getNewData(eigenVector: Array[Double], pixels: Array[Array[Double]]) : Array[Double] = {
    for {
      pixel <- pixels
    } yield (DotP(eigenVector, pixel))
  }

  def generatePictures(in: org.purview.core.data.Matrix[Color]): Array[BufferedImage] = {
    var rgb = splitChannels(in)
    rgb = zeroMean(rgb)

    val covMatrix = covarianceMatrix(rgb)
    val eigVcs = getEigenvectors(covMatrix)

    val pc1 = convertTo2D(getNewData(eigVcs(0), rgb), in.width, in.height)
    val pc2 = convertTo2D(getNewData(eigVcs(1), rgb), in.width, in.height)
    val pc3 = convertTo2D(getNewData(eigVcs(2), rgb), in.width, in.height)

    val result1 = new BufferedImage(in.width, in.height, BufferedImage.TYPE_INT_RGB)
    val result2 = new BufferedImage(in.width, in.height, BufferedImage.TYPE_INT_RGB)
    val result3 = new BufferedImage(in.width, in.height, BufferedImage.TYPE_INT_RGB)

    val g1 = result1.createGraphics()
    val g2 = result2.createGraphics()
    val g3 = result3.createGraphics()

    var y = 0
    while(y < in.height) {
      var x = 0
      while(x < in.width) {
        g1.setColor(new java.awt.Color(pc1(x)(y).toInt, pc1(x)(y).toInt, pc1(x)(y).toInt))
        g2.setColor(new java.awt.Color(pc1(x)(y).toInt, pc1(x)(y).toInt, pc1(x)(y).toInt))
        g3.setColor(new java.awt.Color(pc1(x)(y).toInt, pc1(x)(y).toInt, pc1(x)(y).toInt))

        g1.drawRect(x,y,1,1)
        g2.drawRect(x,y,1,1)
        g3.drawRect(x,y,1,1)

        x += 1
      }
      y += 1
    }

    Array(result1, result2, result3)
  }

  def convertTo2D(gs: Array[Double], w: Int, h: Int): Array[Array[Double]] = {
    var result = new Array[Array[Double]](h)
    for{
      x : Int <- 0 until h
    }{
      result(x) = new Array[Double](w)
    }
    for{
      x : Int <- 0 until gs.length
    }{
      result(x % w)(Math.floor(x / w).toInt) = gs(x)
    }
    result
  }

  def imageReport(img: Array[BufferedImage]): Set[ReportEntry] = {
    Set(new ReportImage(Information, "Principal component #1", 0, 0, img(0)),
        new ReportImage(Information, "Principal component #2", 0, 0, img(1)),
        new ReportImage(Information, "Principal component #3", 0, 0, img(2)))
  }

  val result = input >- generatePictures >- imageReport

}