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

class AnalyserImplementation extends Analyser[ImageMatrix] with Settings {
  val name = "JPEG Ghost Analyser"
  val description = "Finds JPEG ghosts in an image"

  val minQualitySetting = IntRangeSetting("Minimum quality", 1, 100)
  minQualitySetting.value = 50 //default
  val maxQualitySetting = IntRangeSetting("Maximum quality", 1, 100)
  maxQualitySetting.value = 100 //default
  val stepSizeSetting = IntRangeSetting("Step size", 1, 15)
  stepSizeSetting.value = 5 //default

  val settings = List(minQualitySetting, maxQualitySetting, stepSizeSetting)

  private def qmin : Int = minQualitySetting.value
  private def qmax : Int = maxQualitySetting.value
  private def step : Int = stepSizeSetting.value

  private def introduceJPEGArtifacts(in: BufferedImage, q: Int) = {
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
        param.setCompressionQuality((q.toFloat) / 100f)

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

  def createOutputImages(in: BufferedImage) : Array[BufferedImage] = {
    var x = qmin
    var counter = 0
    var output = new Array[BufferedImage](ceil((qmax - qmin) / step).toInt + 1)
    while (x <= qmax)
    {
      output(counter) = computeDifference(in, introduceJPEGArtifacts(in, x))
      counter += 1
      x += step
    }
    output
  }
  
  def computeDifference(in: BufferedImage, compare: BufferedImage): BufferedImage = {
    val w = in.getWidth
    val h = in.getHeight
    var result = new BufferedImage(w,h, BufferedImage.TYPE_INT_ARGB)
    val graphics = result.getGraphics
    graphics.setColor(java.awt.Color.BLACK)
    graphics.clearRect(0,0,w,h)
    for{
      x : Int <- 0 until w
      y : Int <- 0 until h
    }{
      val pixelA = in.getRGB(x, y)
      val pixelB = compare.getRGB(x, y)

      val rA = (pixelA >>> 16)& 255
      val gA = (pixelA >>> 8) & 255
      val bA =  pixelA & 255
      val rB = (pixelB >>> 16)& 255
      val gB = (pixelB >>> 8) & 255
      val bB =  pixelB & 255

      val diffsq = ((1 / 3) * (pow(rA - rB, 2) + pow(gA - gB, 2) + pow(bA - bB, 2)))
      result.setRGB(x, y, ((65536 * diffsq) + (256 * diffsq) + diffsq).toInt)
    }
    result
  }

  def ImageToColorMatrix(img: BufferedImage) = new Matrix[Color] {
    val width = img.getWidth
    val height = img.getHeight
    private val raster = img.getData
    private val buffer = new Array[Int](4)
    def apply(x: Int, y: Int) = synchronized {
      raster.getPixel(x, y, buffer)
      new Color(buffer(3) / 255f, buffer(0) / 255f, buffer(1) / 255f, buffer(2) / 255f)
    }
  }

  def createReport(in: Array[BufferedImage]) : Set[ReportEntry] = {
    (for{ i : Int <- 0 until in.length} yield {
        new ReportImage(Information, "Output at quality " + (qmin + i * step).toString(), 0, 0, ImageToColorMatrix(in(i)))
      }).toSet[ReportEntry]
  } 

  val result = input >- MatrixToImage() >- createOutputImages >- createReport
}
