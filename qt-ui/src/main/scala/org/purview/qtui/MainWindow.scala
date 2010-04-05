package org.purview.qtui

import com.trolltech.qt.core.Qt
import com.trolltech.qt.gui.QAction
import com.trolltech.qt.gui.QApplication
import com.trolltech.qt.gui.QFileDialog
import com.trolltech.qt.gui.QIcon
import com.trolltech.qt.gui.QMainWindow
import com.trolltech.qt.gui.QMenu
import com.trolltech.qt.gui.QMenuBar
import com.trolltech.qt.gui.QMessageBox
import com.trolltech.qt.gui.QTabWidget
import com.trolltech.qt.gui.QToolBar
import java.io.File
import javax.imageio.ImageIO
import org.purview.qtui.meta.ImageSession

object MainWindow extends QMainWindow {
  if(objectName.isEmpty)
    setObjectName("MainWindow")

  setWindowTitle("Purview 1.0")
  setWindowIcon(new QIcon("classpath:icons/purview.png"))

  addDockWidget(Qt.DockWidgetArea.BottomDockWidgetArea, AnalysisView)
  addDockWidget(Qt.DockWidgetArea.LeftDockWidgetArea, ResultsView)

  val tabWidget = new QTabWidget(this) {
    currentChanged.connect(MainWindow.this, "changeSession(int)")
    tabCloseRequested.connect(MainWindow.this, "updateToolbar()")
    tabCloseRequested.connect(MainWindow.this, "closeTab(int)")
    setDocumentMode(true)
    setTabsClosable(true)
  }

  val openImageAction = new QAction(this) {
    setText("&Open Image...")
    setShortcut("Ctrl+N")
    setIcon(new QIcon("classpath:icons/folder-image.png"))
    triggered.connect(MainWindow.this, "selectImage()")
  }

  val exitAction = new QAction(this) {
    setText("E&xit")
    setShortcut("Ctrl+X")
    setIcon(new QIcon("classpath:icons/dialog-error.png"))
    triggered.connect(QApplication.instance(), "quit()")
  }

  val showAnalysisAction = new QAction(this) {
    setText("&Analysis window")
    setShortcut("Ctrl+S")
    setIcon(AnalysisView.windowIcon)
    setCheckable(true)
    setChecked(true)
    toggled.connect(AnalysisView, "setVisible(boolean)")
    AnalysisView.visibilityChanged.connect(this: QAction /*!!*/, "setChecked(boolean)")
  }

  val showResultsAction = new QAction(this) {
    setText("&Results window")
    setShortcut("Ctrl+R")
    setIcon(ResultsView.windowIcon)
    setCheckable(true)
    setChecked(true)
    toggled.connect(ResultsView, "setVisible(boolean)")
    ResultsView.visibilityChanged.connect(this: QAction /*!!*/, "setChecked(boolean)")
  }

  val aboutAction = new QAction(this) {
    setText("&About")
    setIcon(new QIcon("classpath:icons/dialog-information.png"))
    setShortcut("F1")
    triggered.connect(MainWindow.this, "showAboutDialog()")
  }
  
  private val aboutQtAction = new QAction(this) {
    setText("About &Qt")
    setIcon(new QIcon("classpath:icons/qt.png"))
    triggered.connect(MainWindow.this, "showAboutQtDialog()")
  }

  val analyseAction = new QAction(this) {
    setText("Analyse &image")
    setIcon(new QIcon("classpath:icons/system-run.png"))
    setShortcut("Ctrl+A")
    setEnabled(false)
    triggered.connect(MainWindow.this, "analyse()")
  }

  val configureAnalysersAction = new QAction(this) {
    setText("&Configure analysers...")
    setIcon(new QIcon("classpath:icons/configure.png"))
    setShortcut("Ctrl+C")
    setEnabled(false)
    triggered.connect(MainWindow.this, "configureAnalysers()")
  }

  val zoomInAction = new QAction(this) {
    setText("Zoom &in")
    setShortcut("Ctrl++")
    setIcon(new QIcon("classpath:icons/zoom-in.png"))
    setEnabled(false)
    triggered.connect(MainWindow.this, "zoomIn()")
  }

  val zoomOutAction = new QAction(this) {
    setText("Zoom &out")
    setShortcut("Ctrl+-")
    setIcon(new QIcon("classpath:icons/zoom-out.png"))
    setEnabled(false)
    triggered.connect(MainWindow.this, "zoomOut()")
  }

  val zoomOrigAction = new QAction(this) {
    setText("O&riginal size")
    setShortcut("Ctrl+0")
    setIcon(new QIcon("classpath:icons/zoom-original.png"))
    setEnabled(false)
    triggered.connect(MainWindow.this, "zoomOrig()")
  }
  
  val menuFile = new QMenu(this) {
    setTitle("&File")
    addAction(openImageAction)
    addSeparator()
    addAction(exitAction)
  }

  val menuWindow = new QMenu(this) {
    setTitle("&Window")
    addAction(showAnalysisAction)
    addAction(showResultsAction)
  }

  val menuHelp = new QMenu(this) {
    setTitle("&Help")
    addAction(aboutAction)
    addAction(aboutQtAction)
  }

  val mainToolBar = new QToolBar(this) {
    setFloatable(true)
    setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextUnderIcon)
    addAction(openImageAction)
  }
  
  val analysisToolBar = new QToolBar(this) {
    setFloatable(true)
    setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextUnderIcon)
    addAction(analyseAction)
    addAction(configureAnalysersAction)
  }

  val interactToolBar = new QToolBar(this) {
    setFloatable(true)
    setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextUnderIcon)
    addAction(zoomInAction)
    addAction(zoomOutAction)
    addSeparator()
    addAction(zoomOrigAction)
  }

  val menu = new QMenuBar(this) {
    addMenu(menuFile)
    addMenu(menuWindow)
    addMenu(menuHelp)
  }

  addToolBar(mainToolBar)
  addToolBar(analysisToolBar)
  addToolBar(interactToolBar)

  setCentralWidget(tabWidget)
  setMenuBar(menu)

  val fileDiag = new QFileDialog(this)
  fileDiag.setFileMode(QFileDialog.FileMode.ExistingFile)
  fileDiag.setNameFilter(ImageIO.getReaderFileSuffixes.mkString("Image files (*.", " *.", ")"))

  def selectImage() = if(fileDiag.exec() != 0) {
    val filename = fileDiag.selectedFiles.get(0)
    val sessionWidget = new ImageSessionWidget(new ImageSession(new File(filename)))
    tabWidget.addTab(sessionWidget, new QIcon("classpath:icons/image-x-generic.png"), sessionWidget.windowTitle)
    updateToolbar()
  }

  def changeSession(sessionNr: Int) = {
    val imgWidget = tabWidget.widget(sessionNr).asInstanceOf[ImageSessionWidget]
    AnalysisView.analysis = imgWidget.imageSession.analysis
    ResultsView.results = imgWidget.imageSession.analysis.flatMap(_.results)
    imgWidget.imageSession.analysis.foreach(a => ResultsView.results = a.results)
    updateToolbar()
  }

  def closeTab(tab: Int) = {
    tabWidget.widget(tab).close()
    AnalysisView.analysis = None
    ResultsView.results = None
    tabWidget.removeTab(tab)
    updateToolbar()
  }

  def zoomIn() =
    tabWidget.currentWidget.asInstanceOf[ImageSessionWidget].scale(1.25, 1.25)

  def zoomOut() =
    tabWidget.currentWidget.asInstanceOf[ImageSessionWidget].scale(0.8, 0.8)

  def zoomOrig() =
    tabWidget.currentWidget.asInstanceOf[ImageSessionWidget].resetTransform()

  def updateToolbar() {
    val enabled = (tabWidget.currentIndex > -1)
    val imgWidget = Option(tabWidget.currentWidget.asInstanceOf[ImageSessionWidget])
    analyseAction.setEnabled(imgWidget flatMap (_.imageSession.analysis) match {
        case Some(analysis) => analysis.results.isDefined
        case None => true
      })
    configureAnalysersAction.setEnabled(enabled)
    zoomInAction.setEnabled(enabled)
    zoomOutAction.setEnabled(enabled)
    zoomOrigAction.setEnabled(enabled)
  }

  def analyse() = {
    val imgWidget = tabWidget.currentWidget.asInstanceOf[ImageSessionWidget]
    imgWidget.imageSession.analyse()
    imgWidget.imageSession.analysis.foreach(_.finished.connect(this, "refreshResultsView()"))
    AnalysisView.analysis = imgWidget.imageSession.analysis
    updateToolbar()
  }

  def refreshResultsView() = {
    val imgWidget = tabWidget.currentWidget.asInstanceOf[ImageSessionWidget]
    imgWidget.imageSession.analysis.foreach {a =>
      ResultsView.results = a.results
      a.finished.disconnect(this)
    }
  }

  def configureAnalysers() =
    tabWidget.currentWidget.asInstanceOf[ImageSessionWidget].configureAnalysers()
  
  def showAboutDialog() =
    QMessageBox.about(this, "About Purview", "Copyright &copy; 2010 <em>David Flemström</em> and <em>Moritz Roth</em> " +
                      "under the Apache 2.0 license. Please visit <a href=\"http://www.apache.org/licenses/\">" +
                      "http://www.apache.org/licenses/</a> for more information.")

  def showAboutQtDialog() = QMessageBox.aboutQt(this)
}
