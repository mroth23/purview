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
import org.purview.core.report.ReportEntry
import org.purview.core.report.Warning
import org.purview.qtui.meta.Analysis
import scala.collection.mutable

object ResultsView extends QDockWidget {
  setWindowTitle("Results")
  setWindowIcon(QIcon.fromTheme("dialog-ok", new QIcon("classpath:icons/dialog-ok.png")))
  setAllowedAreas(Qt.DockWidgetArea.LeftDockWidgetArea, Qt.DockWidgetArea.RightDockWidgetArea)

  private val treeForResults = new mutable.WeakHashMap[Option[Map[Metadata, Seq[ReportEntry]]], QTreeWidget]
  treeForResults(None) = mkTree(None, None)
  setWidget(treeForResults(None))

  private var _analysis: Option[Analysis] = None
  def analysis = _analysis
  def analysis_=(maybeAnalysis: Option[Analysis]) = {
    val maybeResults = maybeAnalysis.flatMap(_.results)
    setWidget(treeForResults.getOrElseUpdate(maybeResults,
                                             mkTree(maybeResults, maybeAnalysis)))
    _analysis = maybeAnalysis
  }

  private def mkTree(report: Option[Map[Metadata, Seq[ReportEntry]]], analysis: Option[Analysis]) = new QTreeWidget(this) {
    setColumnCount(1)
    setAlternatingRowColors(true)
    setAnimated(true)
    setHeaderHidden(true)
    setSelectionMode(QAbstractItemView.SelectionMode.SingleSelection)
    report.foreach(mkTreeItem)
    itemSelectionChanged.connect(this, "changeNode()")

    def mkTreeItem(report: Map[Metadata, Seq[ReportEntry]]) =
      for(analyser <- report.keySet.toSeq.sortBy(_.name)) yield {
        val analyserItem = new QTreeWidgetItem(this) {
          setText(0, analyser.name)
          setIcon(0, QIcon.fromTheme("dialog-ok", new QIcon("classpath:icons/dialog-ok.png")))
          setData(0, Qt.ItemDataRole.ToolTipRole, analyser.description)
        }
        for(entry <- report(analyser).sortBy(_.level.name).sortBy(_.message)) {
          val reportItem = new QTreeWidgetItem(analyserItem) {
            setData(0, Qt.ItemDataRole.ToolTipRole, entry.level.name)
            setData(0, Qt.ItemDataRole.UserRole, entry)
            setText(0, entry.message)
            entry.level match {
              case Debug =>       setIcon(0, QIcon.fromTheme("security-high", new QIcon("classpath:icons/security-high.png")))
              case Information => setIcon(0, QIcon.fromTheme("dialog-information", new QIcon("classpath:icons/dialog-information.png")))
              case Warning =>     setIcon(0, QIcon.fromTheme("dialog-warning", new QIcon("classpath:icons/dialog-warning.png")))
              case Error =>       setIcon(0, QIcon.fromTheme("dialog-error", new QIcon("classpath:icons/dialog-error.png")))
              case Critical =>    setIcon(0, QIcon.fromTheme("security-low", new QIcon("classpath:icons/security-low.png")))
              case _ =>           setIcon(0, QIcon.fromTheme("security-medium", new QIcon("classpath:icons/security-medium.png")))
            }
          }
        }
        addTopLevelItem(analyserItem)
      }

    def changeNode() = if(selectedItems.size > 0)
      analysis.foreach(_.changeReportEntry(Option(selectedItems.get(0).data(0, Qt.ItemDataRole.UserRole).asInstanceOf[ReportEntry])))
  }
}
