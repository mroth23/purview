package org.purview.jpegghostanalyser

import java.awt.image.BufferedImage
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import javax.imageio.IIOImage
import javax.imageio.ImageIO
import javax.imageio.ImageWriteParam
import org.purview.core.analysis._
import org.purview.core.analysis.settings._
import org.purview.core.data._
import org.purview.core.report._
import org.purview.core.transforms._
import scala.math._

class AnalyserImplementation extends HeatMapImageAnalyser with Settings {
  val name = "JPEG Ghost Analyser"
  val description = "Finds JPEG ghosts in an image"

  val minQualitySetting = FloatRangeSetting("Minimum quality", 0, 1, 100)
  minQualitySetting.value = 0.5f //default
  val maxQualitySetting = FloatRangeSetting("Maximum quality", 0, 1, 100)
  maxQualitySetting.value = 1.0f //default
  val stepSizeSetting = FloatRangeSetting("Step size", 0, 1, 100)
  stepSizeSetting.value = 0.05f //default

  val thr = 0.01f

  val settings = List(minQualitySetting, maxQualitySetting, stepSizeSetting)

  private def qmin = minQualitySetting.value
  private def qmax = maxQualitySetting.value
  private def qstep = stepSizeSetting.value

  private def introduceJPEGArtifacts(in: BufferedImage, q: Float) = {
    status("Generating JPEG artifacts")
    val writers = ImageIO.getImageWritersByFormatName("jpeg")

    val out = new ByteArrayOutputStream
    var result: Option[BufferedImage] = None

    while(result.isEmpty && writers.hasNext) {
      val writer = writers.next()
      try {
        val rgb = new BufferedImage(in.getWidth, in.getHeight,
                                    BufferedImage.TYPE_INT_RGB)
        val g = rgb.createGraphics
        try g.drawImage(in, 0, 0, null) finally g.dispose()

        writer.setOutput(ImageIO.createImageOutputStream(out))

        val param = writer.getDefaultWriteParam
        param.setCompressionMode(ImageWriteParam.MODE_EXPLICIT)
        param.setCompressionQuality(q)

        writer.write(null, new IIOImage(rgb, null, null), param)

        val destroyedImg =
          ImageIO.read(new ByteArrayInputStream(out.toByteArray))

        val argb = new BufferedImage(in.getWidth, in.getHeight,
                                     BufferedImage.TYPE_INT_ARGB)
        val g2 = argb.createGraphics
        try g2.drawImage(destroyedImg, 0, 0, null) finally g2.dispose()

        result = Some(argb)
      } catch {
        case _ => //ignore
      } finally {
        writer.dispose()
        out.close()
      }
    }

    result getOrElse {
      status("Couldn't find any working JPEG writers")
      in
    }
  }

  def range(start: Float, end: Float, step: Float) =
    for(i <- 0 until ((end - start) / step).toInt) yield start + step * i

  val inputAsBufferedImage = for(in <- input) yield MatrixToImage()(in)

  val outputImages = for(in <- inputAsBufferedImage) yield (for(q <- range(qmin, qmax, qstep)) yield introduceJPEGArtifacts(in, q))

  val outputImagesAsColorMatrices = for(in <- outputImages) yield in map ImageToMatrix()

  val computeDifferences = for(in <- input; imgs <- outputImagesAsColorMatrices) yield
    (for(img <- imgs) yield img zip in map (imgPair => abs(imgPair._1.weight - imgPair._2.weight)))

  val heatmap = for(in <- computeDifferences; orig <- input) yield {
    val imgsWithQuality = in zip range(qmin, qmax, qstep)
    val result = new MutableArrayMatrix[Float](orig.width, orig.height)

    for((img, quality) <- imgsWithQuality) {
      img.cells.foreach { tmp =>
        val (x, y, v) = tmp
        if(result(x, y) < qmin && v > thr)
          result(x, y) = quality
      }
    }
    result: Matrix[Float]
  }
}
