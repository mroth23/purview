package org.purview.principalcomponentanalyser

import org.purview.core.report._
import org.purview.core.transforms._
import org.purview.core.data.ImmutableMatrix
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

  type ThreeFloatMatrices = (Matrix[Float], Matrix[Float], Matrix[Float])

  def splitChannels(in: org.purview.core.data.Matrix[Color]): ThreeFloatMatrices =
    (in.map(_.r), in.map(_.g), in.map(_.b))

  def zeroMean(rgb: ThreeFloatMatrices): ThreeFloatMatrices = {
    val (r, g, b) = rgb
    val meanr = r.sum / (r.width * r.height)
    val meang = g.sum / (g.width * g.height)
    val meanb = b.sum / (b.width * b.height)

    (r map (_ - meanr), g map (_ - meang), b map (_ - meanb))
  }

  def covariance(a1: Matrix[Float], a2: Matrix[Float]): Float = {
    val result = a1.zip(a2).foldLeft(0f) ((acc, elem) => acc + elem._1 * elem._2)
    (result / (a1.width * a1.height)).toFloat
  }

  def covarianceMatrix(rgb: ThreeFloatMatrices): Matrix[Float] = {
    val (r, g, b) = rgb

    val covRR = covariance(r, r)
    val covRG = covariance(r, g)
    val covRB = covariance(r, b)

    val covGG = covariance(g, g)
    val covGB = covariance(g, b)

    val covBB = covariance(b, b)

    new ImmutableMatrix(3, 3, Array(
        covRR, covRG, covRB,
        covRG, covGG, covGB,
        covRB, covGB, covBB
     ))
  }

  def getEigenvectors(in: Matrix[Float]): Matrix[Float] = new EigenvalueDecomposition(in).vectors

  def dotProduct(vector1: (Float, Float, Float), vector2: (Float, Float, Float)) =
    vector1._1 * vector2._1 + vector1._2 * vector2._2 + vector1._3 * vector2._3

  def getNewData(eigenVector: Seq[Double], pixels: ThreeFloatMatrices): Array[Double] = {
    for {
      pixel <- pixels
    } yield (dotProduct(eigenVector, pixel))
  }

  def generatePictures(in: Matrix[Color]): Array[BufferedImage] = {
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