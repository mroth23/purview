package org.purview.core.report

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import org.apache.commons.codec.binary.Base64
import org.purview.core.analysis.Metadata
import scala.xml.Elem

object ReportPersistance {
  def encodeReportTree(reportTree: Map[Metadata, Set[ReportEntry]]) = {
    val analysers = for(metadata <- reportTree.keySet) yield
      (<analyser name={metadata.name} description={metadata.description} author={metadata.author.orNull} version={metadata.version.orNull}>{
            reportTree(metadata).map(encodeReportEntry).toSeq
          }</analyser>)

    <purviewreport version="1.0">{analysers}</purviewreport>
  }
  
  def encodeReportEntry(entry: ReportEntry) = {
    val out = new ByteArrayOutputStream
    try {
      val writer = new ObjectOutputStream(out)
      writer.writeObject(entry)
      writer.flush()
      writer.close()
    } finally {
      out.close()
    }
    <reportentry>{new String(Base64.encodeBase64(out.toByteArray), "UTF-8")}</reportentry>
  }

  def decodeReportTree(reportTree: Elem): Map[Metadata, Set[ReportEntry]] =
    (for(analyser <- reportTree\"analyser") yield {
        val metadata: Metadata = new Metadata {
          val name = (analyser\"@name").text
          val description = (analyser\"@description").text
          override val author = Some((analyser\"@author").text) //TODO: fix null
          override val version = Some((analyser\"@version").text)
        }
        val entries = ((analyser\"reportentry") partialMap { case e: Elem => decodeReportEntry(e) }).toSet
        (metadata, entries)
      }).toMap

  def decodeReportEntry(reportentry: Elem) = {
    val in = new ByteArrayInputStream(Base64.decodeBase64(reportentry.text.getBytes("UTF-8")))
    try {
      val reader = new ObjectInputStream(in)
      reader.readObject.asInstanceOf[ReportEntry]
    } finally {
      in.close()
    }
  }
}
