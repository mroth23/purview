package org.purview.qtui

import com.trolltech.qt.core.Qt
import com.trolltech.qt.gui.QAbstractItemView
import com.trolltech.qt.gui.QDockWidget
import com.trolltech.qt.gui.QIcon
import com.trolltech.qt.gui.QTreeWidget
import com.trolltech.qt.gui.QTreeWidgetItem
import org.purview.core.analysis.Metadata
import org.purview.core.report.Critical
import org.purview.core.report.Debug
import org.purview.core.report.Error
import org.purview.core.report.Information
import org.purview.core.report.Message
import org.purview.core.report.ReportEntry
import org.purview.core.report.Warning
import scala.collection.mutable

object ResultsView extends QDockWidget {
  val reportEntryChanged = new Signal1[Option[ReportEntry]]

  setWindowTitle("Results")
  setWindowIcon(new QIcon("classpath:icons/dialog-ok.png"))
  setAllowedAreas(Qt.DockWidgetArea.LeftDockWidgetArea, Qt.DockWidgetArea.RightDockWidgetArea)

  private val treeWidget = new QTreeWidget(this)
  treeWidget.setColumnCount(1)
  treeWidget.setAlternatingRowColors(true)
  treeWidget.setAnimated(true)
  treeWidget.setHeaderHidden(true)
  treeWidget.setSelectionMode(QAbstractItemView.SelectionMode.SingleSelection)
  treeWidget.itemSelectionChanged.connect(this, "changeNode()")
  setWidget(treeWidget)

  private var _results: Option[Map[Metadata, Seq[ReportEntry]]] = None
  def results = _results
  def results_=(maybeResults: Option[Map[Metadata, Seq[ReportEntry]]]) = {
    treeWidget.clear()

    def mkTree(report: Map[Metadata, Seq[ReportEntry]]) =
      for(analyser <- report.keySet.toSeq.sortWith(_.name < _.name)) yield {
        val analyserItem = new QTreeWidgetItem(treeWidget)
        analyserItem.setText(0, analyser.name)
        analyserItem.setIcon(0, new QIcon("classpath:icons/dialog-ok.png"))
        analyserItem.setData(0, Qt.ItemDataRole.ToolTipRole, analyser.description)
        for(entry <- report(analyser)) {
          val reportItem = new QTreeWidgetItem(analyserItem)
          reportItem.setData(0, Qt.ItemDataRole.ToolTipRole, entry.level.name)
          reportItem.setData(0, Qt.ItemDataRole.UserRole, entry)
          entry match {
            case m: Message =>
              reportItem.setText(0, m.message)
            case _ =>
              reportItem.setText(0, "?")
          }
          entry.level match {
            case Debug =>
              reportItem.setIcon(0, new QIcon("classpath:icons/security-high.png"))
            case Information =>
              reportItem.setIcon(0, new QIcon("classpath:icons/dialog-information.png"))
            case Warning =>
              reportItem.setIcon(0, new QIcon("classpath:icons/dialog-warning.png"))
            case Error =>
              reportItem.setIcon(0, new QIcon("classpath:icons/dialog-error.png"))
            case Critical =>
              reportItem.setIcon(0, new QIcon("classpath:icons/security-low.png"))
            case _ =>
              reportItem.setIcon(0, new QIcon("classpath:icons/security-medium.png"))
          }
        }
        analyserItem
      }

    for (report <- maybeResults) {
      mkTree(report).foreach(treeWidget.addTopLevelItem)
    }
  }

  private def changeNode() {
    val selectedList = treeWidget.selectedItems
    if(selectedList.size > 0) {
      val entry = selectedList.get(0).data(0, Qt.ItemDataRole.UserRole).asInstanceOf[ReportEntry]
      
      reportEntryChanged.emit(Option(entry))
    }
  }
}
