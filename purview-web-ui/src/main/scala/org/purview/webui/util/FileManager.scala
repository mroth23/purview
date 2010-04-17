package org.purview.webui.util

import java.awt.image.BufferedImage
import java.awt.image.RenderedImage
import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import javax.imageio.ImageIO
import net.liftweb.util.Helpers
import net.liftweb.util.Props
import org.purview.core.analysis.Metadata
import org.purview.core.report.ReportEntry
import org.purview.core.report.ReportPersistance
import scala.xml.XML

object FileManager {
  private val baseDirectory = Props.get("filesystem.dir") map (new File(_)) openOr
  new File(new File(System.getProperty("java.io.tmpdir")), "purview")

  if(!baseDirectory.exists)
    baseDirectory.mkdirs()
}

abstract class FileManager {
  import FileManager._

  protected val kind: String
  protected val suffix: String

  def file(id: String) = {
    val subdir = new File(baseDirectory, kind)

    if(!subdir.exists)
      subdir.mkdirs()

    new File(subdir, id + "." + suffix)
  }
}

object PersistanceProvider {
  private def randomStrings: Stream[String] = Stream.cons(Helpers.randomString(16), randomStrings)
}

trait PersistanceProvider[A] extends FileManager {
  import PersistanceProvider._

  def read(id: String): Option[A]
  def write(id: String, data: A): Unit

  def exists(id: String) = file(id).exists
  def makeId = randomStrings.dropWhile(exists _).head
}

object ImageManager extends FileManager with PersistanceProvider[RenderedImage] {
  val kind = "image"
  val suffix = "png"
  def read(id: String): Option[BufferedImage] = try Some(ImageIO.read(file(id))) catch {case _ => None}
  def write(id: String, image: RenderedImage) = ImageIO.write(image, "png", file(id))
}

object UploadManager extends FileManager with PersistanceProvider[Nothing] {
  val kind = "upload"
  val suffix = "bin"
  def read(id: String) = error("This is a generic file handler; can't read")
  def write(id: String, data: Nothing) = ()
}

object ReportManager extends FileManager with PersistanceProvider[Map[Metadata, Set[ReportEntry]]] {
  val kind = "report"
  val suffix = "xml"
  def read(id: String) =
    try Some(ReportPersistance.deserializeReport(XML.loadFile(file(id)))) catch {
      case _ => None
    }
  def write(id: String, report: Map[Metadata, Set[ReportEntry]]) =
    XML.save(file(id).getAbsolutePath, ReportPersistance.serializeReport(report), "UTF-8", true)
}
