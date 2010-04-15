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
  type ThreeImages = (BufferedImage, BufferedImage, BufferedImage)

  def splitChannels(in: org.purview.core.data.Matrix[Color]): ThreeFloatMatrices =
    (in.map(_.r), in.map(_.g), in.map(_.b))

  def mean(matrix: Matrix[Float]): Float = matrix.sum / (matrix.width * matrix.height)

  def zeroMean(rgb: ThreeFloatMatrices): ThreeFloatMatrices = {
    val (r, g, b) = rgb
    val meanr = mean(r)
    val meang = mean(g)
    val meanb = mean(b)

    (r map (_ - meanr), g map (_ - meang), b map (_ - meanb))
  }

  def covariance(a1: Matrix[Float], a2: Matrix[Float]): Float = {
    val result = a1.zip(a2).foldLeft(0f) ((acc, elem) => acc + elem._1 * elem._2)
    (result / (a1.width * a1.height - 1)).toFloat
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

  def getNewData(eigenVector: Int => Float, cells: ThreeFloatMatrices): Matrix[Float] = {
    val vector = (eigenVector(0), eigenVector(1), eigenVector(2))
    for {
      (x, y, value1) <- cells._1.cells
      value2 = cells._2(x, y)
      value3 = cells._3(x, y)
      cell = (value1, value2, value3)
    } yield (dotProduct(vector, cell))
  }

  def generatePictures(in: Matrix[Color]): ThreeImages = {
    val rgb = zeroMean(splitChannels(in))

    val eigVcs = getEigenvectors(covarianceMatrix(rgb))

    val pc1 = getNewData(eigVcs(0, _), rgb)
    val pc2 = getNewData(eigVcs(1, _), rgb)
    val pc3 = getNewData(eigVcs(2, _), rgb)

    val result1 = new BufferedImage(in.width, in.height, BufferedImage.TYPE_INT_RGB)
    val result2 = new BufferedImage(in.width, in.height, BufferedImage.TYPE_INT_RGB)
    val result3 = new BufferedImage(in.width, in.height, BufferedImage.TYPE_INT_RGB)

    var y = 0
    while(y < in.height) {
      var x = 0
      while(x < in.width) {
        result1.setRGB(x, y, new java.awt.Color(pc1(x, y).toInt, pc1(x, y).toInt, pc1(x, y).toInt).getRGB)
        result2.setRGB(x, y, new java.awt.Color(pc1(x, y).toInt, pc1(x, y).toInt, pc1(x, y).toInt).getRGB)
        result3.setRGB(x, y, new java.awt.Color(pc1(x, y).toInt, pc1(x, y).toInt, pc1(x, y).toInt).getRGB)

        x += 1
      }
      y += 1
    }

    (result1, result2, result3)
  }

  def imageReport(img: ThreeImages): Set[ReportEntry] = {
    Set(new ReportImage(Information, "Principal component #1", 0, 0, img._1),
        new ReportImage(Information, "Principal component #2", 0, 0, img._2),
        new ReportImage(Information, "Principal component #3", 0, 0, img._3))
  }

  val result = input >- generatePictures >- imageReport

}