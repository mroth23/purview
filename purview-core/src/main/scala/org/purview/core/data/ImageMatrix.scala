package org.purview.core.data

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import scala.collection.JavaConversions._
import scala.xml.XML

object ImageMatrix {
  def fromFile(imageFile: File, metadataReaders: Seq[meta.MetadataReader] = meta.MetadataReaders.default): ImageMatrix = {
    val stream = ImageIO.createImageInputStream(imageFile)
    try {
      val readers = ImageIO.getImageReaders(stream)
      if (!readers.hasNext)
        throw new IllegalArgumentException("No readers found for the specified image")

      val reader = readers.next()
      reader.setInput(stream)

      val metadata: Map[String, Map[String, String]] = metadataReaders.flatMap(_.mkNode(imageFile, reader.getFormatName)).toMap

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

package meta {
  object MetadataReaders {
    val default: Seq[MetadataReader] = Seq(ExifMetadataReader, IptcMetadataReader, DQTMetadataReader, XmpMetadataReader)
  }

  trait MetadataReader {
    val key: String
    def read: PartialFunction[(File, String), Map[String, String]]
    def mkNode(file: File, format: String) =
      read.lift(file, format).map(key -> _)
  }

  object ExifMetadataReader extends MetadataReader {
    import com.drew.imaging.jpeg.JpegMetadataReader
    import com.drew.metadata.exif.ExifDirectory
    import com.drew.metadata.Tag
    val key = "Exif"
    def read = {
      case (file, "JPEG") =>
        val meta = JpegMetadataReader.readMetadata(file)
        val dir = meta.getDirectory(classOf[ExifDirectory])
        if(dir == null) Map.empty else {
          val tags = for {
            tag <- dir.getTagIterator.asInstanceOf[java.util.Iterator[Tag]]
            if tag != null
            if tag.getTagName != null
          } yield (tag.getTagName, tag.getDescription)
          tags.toMap
        }
    }
  }

  object IptcMetadataReader extends MetadataReader {
    import com.drew.imaging.jpeg.JpegMetadataReader
    import com.drew.metadata.iptc.IptcDirectory
    import com.drew.metadata.Tag
    val key = "Iptc"
    def read = {
      case (file, "JPEG") =>
        val meta = JpegMetadataReader.readMetadata(file)
        val dir = meta.getDirectory(classOf[IptcDirectory])
        if(dir == null) Map.empty else {
          val tags = for {
            tag <- dir.getTagIterator.asInstanceOf[java.util.Iterator[Tag]]
            if tag != null
            if tag.getTagName != null
          } yield (tag.getTagName, tag.getDescription)
          tags.toMap
        }
    }
  }

  object DQTMetadataReader extends MetadataReader {
    import com.drew.imaging.jpeg.JpegSegmentReader
    val key = "DQT"
    def read = {
      case (file, "JPEG") =>
        val segmentReader = new JpegSegmentReader(file)
        val numberOfDQTSegments = segmentReader.getSegmentCount(JpegSegmentReader.SEGMENT_DQT)
        val indexedQTables = for {
          i <- 0 until numberOfDQTSegments
          segment = segmentReader.readSegment(JpegSegmentReader.SEGMENT_DQT, i).map(_ & 0xff)
          qtable <- segment.grouped(65).map(_.toSeq).toSeq.filter(_.length == 65)
        } yield qtable(0).toString -> qtable.drop(1).mkString(",")
        indexedQTables.toMap
    }
  }

  object XmpMetadataReader extends MetadataReader {
    import com.drew.imaging.jpeg.JpegSegmentReader
    val key = "Xmp"
    def read = {
      case (file, "JPEG") =>
        val segmentReader = new JpegSegmentReader(file)
        val numberOfAppSegments = segmentReader.getSegmentCount(JpegSegmentReader.SEGMENT_APP1)
        (for {
            i <- 0 until numberOfAppSegments
            segment = new String(segmentReader.readSegment(JpegSegmentReader.SEGMENT_APP1, i), "UTF-8")
            if segment startsWith "http://ns.adobe.com/xap/1.0/"
            xmpmeta = XML.loadString(segment.substring(29))
            elem <- xmpmeta\"RDF"\"Description"\"_"
          } yield elem.label match {
            case "History" =>
              for(event <- elem\"Seq"\"li") yield {
                "history:" + (event\"@{http://ns.adobe.com/xap/1.0/sType/ResourceEvent#}action").text ->
                (event\"@{http://ns.adobe.com/xap/1.0/sType/ResourceEvent#}softwareAgent").text
              }
            case "WhitePoint" => Nil
            case "PrimaryChromaticities" => Nil
            case "YCbCrCoefficients" => Nil
            case "ComponentsConfiguration" => Nil
            case "ISOSpeedRatings" => Nil
            case "Flash" => Nil
            case ign => println("WARN: Ignored RDF entry " + ign); Nil
          }).flatten.toMap
    }
  }
}
