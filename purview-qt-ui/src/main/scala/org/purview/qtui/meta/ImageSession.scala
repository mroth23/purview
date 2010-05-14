package org.purview.qtui.meta

import com.trolltech.qt.QSignalEmitter
import java.io.File
import org.purview.core.data.ImageMatrix
import org.purview.core.report.ReportEntry
import org.purview.core.session.SessionUtils

case class ImageSession(imageFile: File) extends QSignalEmitter {
  val reportEntryChanged = new Signal1[Option[ReportEntry]]

  val matrix = ImageMatrix.fromFile(imageFile)
  var analysers = SessionUtils.createAnalyserInstances[ImageMatrix]().map(_ -> false).toMap

  @volatile private var _reportEntry: Option[ReportEntry] = None
  def reportEntry = _reportEntry
  def reportEntry_=(reportEntry: Option[ReportEntry]) = if(_reportEntry != reportEntry) {
    _reportEntry = reportEntry
    reportEntryChanged.emit(reportEntry)
  }

  @volatile private var _analysis: Option[Analysis] = None
  def analysis = _analysis

  def analyse() = {
    val anas = analysers.keySet.filter(analysers).toSeq
    val a = new Analysis(matrix, imageFile.getName, anas)
    a.analyse() //Asynchronous
    _analysis = Some(a)
  }
}
