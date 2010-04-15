package org.purview.core.data

import com.drew.imaging.jpeg.JpegMetadataReader
import com.drew.imaging.jpeg.JpegSegmentReader
import com.drew.metadata.Directory
import com.drew.metadata.Tag
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import scala.collection.JavaConversions._

object ImageMatrix {
  def fromFile(imageFile: File): ImageMatrix = {
    val stream = ImageIO.createImageInputStream(imageFile)
    try {
      val readers = ImageIO.getImageReaders(stream);
      if (!readers.hasNext)
        throw new IllegalArgumentException("No readers found for the specified image");

      val reader = readers.next()
      reader.setInput(stream)

      val metadata: Map[String, Map[String, String]] =
        if("JPEG" == reader.getFormatName) {
          val meta = JpegMetadataReader.readMetadata(imageFile)
          val metaTree = for {
            dir <- meta.getDirectoryIterator.asInstanceOf[java.util.Iterator[Directory]]
            if dir != null
            if dir.getName != null
          } yield {
            val tags = for {
              tag <- dir.getTagIterator.asInstanceOf[java.util.Iterator[Tag]]
              if tag != null
              if tag.getTagName != null
            } yield (tag.getTagName, tag.getDescription)
            (dir.getName, tags.toSeq.toMap)
          }

          val segmentReader = new JpegSegmentReader(imageFile)
          val numberOfSegments = segmentReader.getSegmentCount(JpegSegmentReader.SEGMENT_DQT)
          val quantMap = (for(i <- 0 until numberOfSegments) yield
            i.toString -> segmentReader.readSegment(JpegSegmentReader.SEGMENT_DQT, i).mkString(",")).toMap

          metaTree.toSeq.toMap + ("QDT" -> quantMap)
        } else Map.empty

      val raw = reader.read(0)
      val argbVersion = new BufferedImage(raw.getWidth, raw.getHeight, BufferedImage.TYPE_INT_ARGB)
      val g = argbVersion.createGraphics
      g.drawImage(raw, 0, 0, null)
      g.dispose()

      new ImageMatrix(argbVersion, metadata)
    } finally stream.close()
  }
}

/**
 * A matrix that is specialized for images, and stores values very efficiently
 */
class ImageMatrix(val image: BufferedImage, val metadata: Map[String, Map[String, String]]) extends Matrix[Color] {

  val width = image.getWidth
  val height = image.getHeight
  private val raster = image.getData
  private val buffer = new Array[Int](4)
  def apply(x: Int, y: Int) = synchronized {
    raster.getPixel(x, y, buffer)
    new Color(buffer(3) / 255f, buffer(0) / 255f, buffer(1) / 255f, buffer(2) / 255f)
  }
}
