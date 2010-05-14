package org.purview.errorlevelanalyser

import java.awt.image.BufferedImage
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import javax.imageio.IIOImage
import javax.imageio.ImageIO
import javax.imageio.ImageWriteParam
import org.purview.core.analysis.HeatMapImageAnalyser
import org.purview.core.analysis.Settings
import org.purview.core.analysis.settings.FloatRangeSetting
import org.purview.core.data.Computation
import org.purview.core.report._
import org.purview.core.transforms.ImageToMatrix
import org.purview.core.transforms.MatrixToImage
import scala.math._

class AnalyserImplementation extends HeatMapImageAnalyser
                                with Settings {
  val name = "Error-level analyser"
  val description = "Uses compression errors in JPEG images to find modification errors"
  override val version = Some("1.1")
  override val author = Some("Moritz Roth & David Flemström")

  val qualitySetting = new FloatRangeSetting("Quality level", 0, 1, 100)
  qualitySetting.value = 0.2f //default

  private def quality: Float = qualitySetting.value
  
  val settings = List(qualitySetting)

  override val message = "Noticeable compression artifact"
  override val reportLevel = Warning
  //We implement our own threshold and don't want upstream interference
  //from the HeatMapAnalyser
  override val threshold = 0f
  override val maxDeviationTolerance = 0.5f

  /** Recompress the given image with a built-in JPEG codec */
  private def introduceJPEGArtifacts(in: BufferedImage) = {
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
        param.setCompressionQuality(quality)

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

  private val errorMatrix = input >- MatrixToImage() >-
    introduceJPEGArtifacts >- ImageToMatrix()

  val diff = for(in <- input; artifacts <- errorMatrix) yield
    in.zip(artifacts).map(x => (x._1 - x._2).weight / 2)

  val heatmap = diff
}
