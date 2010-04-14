package org.purview.core.report

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.InputStream
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.io.OutputStream

object ReportPersistance {
  def serializeEntry(entry: ReportEntry): Array[Byte] = {
    val out = new ByteArrayOutputStream
    try serializeEntry(entry, out) finally out.close()
    out.toByteArray
  }

  def serializeEntry(entry: ReportEntry, out: OutputStream) = {
    val writer = new ObjectOutputStream(out)
    try writer.writeObject(entry) finally writer.close()
  }

  def deserializeEntry(data: Array[Byte]): ReportEntry = {
    val in = new ByteArrayInputStream(data)
    try deserializeEntry(in) finally in.close()
  }

  def deserializeEntry(data: InputStream): ReportEntry = {
    val reader = new ObjectInputStream(data)
    try reader.readObject.asInstanceOf[ReportEntry] finally reader.close()
  }
}
