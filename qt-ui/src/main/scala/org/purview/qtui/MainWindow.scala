package org.purview.qtui

import com.trolltech.qt.core.QDir
import com.trolltech.qt.core.QObject
import com.trolltech.qt.core.QSignalMapper
import com.trolltech.qt.core.Qt
import com.trolltech.qt.gui.QAction
import com.trolltech.qt.gui.QApplication
import com.trolltech.qt.gui.QFileDialog
import com.trolltech.qt.gui.QIcon
import com.trolltech.qt.gui.QMainWindow
import com.trolltech.qt.gui.QMenu
import com.trolltech.qt.gui.QMenuBar
import com.trolltech.qt.gui.QMessageBox
import com.trolltech.qt.gui.QPixmap
import com.trolltech.qt.gui.QTabWidget
import com.trolltech.qt.gui.QToolBar
import java.io.File
import javax.imageio.ImageIO
import org.purview.core.analysis.Analyser
import org.purview.core.data.ImageMatrix
import org.purview.core.session.SessionUtils
import org.purview.qtui.meta.ImageSession

object MainWindow extends QMainWindow {
  if(objectName.isEmpty)
    setObjectName("MainWindow")

  setWindowTitle("Purview 1.0")
  setWindowIcon(new QIcon("classpath:icons/purview.png"))
  setMinimumSize(800, 600)

  addDockWidget(Qt.DockWidgetArea.BottomDockWidgetArea, AnalysisView)
  AnalysisView.hide()
  addDockWidget(Qt.DockWidgetArea.LeftDockWidgetArea, ResultsView)
  ResultsView.hide()

  private val shallowAnalysers = SessionUtils.createAnalyserInstances[ImageMatrix]

  private val tabWidget = new QTabWidget(this) {
    currentChanged.connect(MainWindow.this, "changeSession(int)")
    tabCloseRequested.connect(MainWindow.this, "updateToolbar()")
    tabCloseRequested.connect(MainWindow.this, "closeTab(int)")
    setDocumentMode(true)
    setTabsClosable(true)
  }

  private val openImageAction = new QAction(this) {
    setText("&Open Image...")
    setShortcut("Ctrl+N")
    setIcon(new QIcon("classpath:icons/folder-image.png"))
    triggered.connect(MainWindow.this, "selectImage()")
  }

  private val exitAction = new QAction(this) {
    setText("E&xit")
    setShortcut("Ctrl+X")
    setIcon(new QIcon("classpath:icons/dialog-error.png"))
    triggered.connect(QApplication.instance(), "quit()")
  }

  private val showAnalysisAction = new QAction(this) {
    setText("&Analysis window")
    setShortcut("Ctrl+S")
    setIcon(AnalysisView.windowIcon)
    setCheckable(true)
    setChecked(true)
    toggled.connect(AnalysisView, "setVisible(boolean)")
    AnalysisView.visibilityChanged.connect(this: QAction /*!!*/, "setChecked(boolean)")
  }

  private val showResultsAction = new QAction(this) {
    setText("&Results window")
    setShortcut("Ctrl+R")
    setIcon(ResultsView.windowIcon)
    setCheckable(true)
    setChecked(true)
    toggled.connect(ResultsView, "setVisible(boolean)")
    ResultsView.visibilityChanged.connect(this: QAction /*!!*/, "setChecked(boolean)")
  }

  private val aboutAction = new QAction(this) {
    setText("&About Purview")
    setIcon(new QIcon("classpath:icons/dialog-information.png"))
    setShortcut("F1")
    triggered.connect(MainWindow.this, "showAboutDialog()")
  }
  
  private val aboutQtAction = new QAction(this) {
    setText("About &Qt")
    setIcon(new QIcon("classpath:icons/qt.png"))
    triggered.connect(MainWindow.this, "showAboutQtDialog()")
  }

  private val analyseAction = new QAction(this) {
    setText("Analyse &image")
    setIcon(new QIcon("classpath:icons/system-run.png"))
    setShortcut("Ctrl+A")
    setEnabled(false)
    triggered.connect(MainWindow.this, "analyse()")
  }

  private val configureAnalysersAction = new QAction(this) {
    setText("&Configure analysers...")
    setIcon(new QIcon("classpath:icons/configure.png"))
    setShortcut("Ctrl+C")
    setEnabled(false)
    triggered.connect(MainWindow.this, "configureAnalysers()")
  }

  private val zoomInAction = new QAction(this) {
    setText("Zoom &in")
    setShortcut("Ctrl++")
    setIcon(new QIcon("classpath:icons/zoom-in.png"))
    setEnabled(false)
    triggered.connect(MainWindow.this, "zoomIn()")
  }

  private val zoomOutAction = new QAction(this) {
    setText("Zoom &out")
    setShortcut("Ctrl+-")
    setIcon(new QIcon("classpath:icons/zoom-out.png"))
    setEnabled(false)
    triggered.connect(MainWindow.this, "zoomOut()")
  }

  private val zoomOrigAction = new QAction(this) {
    setText("O&riginal size")
    setShortcut("Ctrl+0")
    setIcon(new QIcon("classpath:icons/zoom-original.png"))
    setEnabled(false)
    triggered.connect(MainWindow.this, "zoomOrig()")
  }

  private val analyserMapper = new QSignalMapper(this) {
    this.mappedQObject.connect(MainWindow.this, "analyserInfoClicked(QObject)")
  }

  private val analyserActions = for(analyser <- shallowAnalysers) yield
    new QAction(this) {
      setIcon(new QIcon("classpath:" + (analyser.iconResource getOrElse "icons/system-run.png")))
      setText("About \"" + analyser.name + "\"")
      setToolTip(analyser.description)
      setData(analyser)
      analyserMapper.setMapping(this, this)
      triggered.connect(analyserMapper, "map()")
    }
  
  private val menuFile = new QMenu(this) {
    setTitle("&File")
    addAction(openImageAction)
    addAction(analyseAction)
    addAction(configureAnalysersAction)
    addSeparator()
    addAction(exitAction)
  }

  private val menuWindow = new QMenu(this) {
    setTitle("&Window")
    addAction(showAnalysisAction)
    addAction(showResultsAction)
    addSeparator()
    addAction(zoomInAction)
    addAction(zoomOutAction)
    addAction(zoomOrigAction)
  }

  private val menuHelp = new QMenu(this) {
    setTitle("&Help")
    analyserActions.foreach(addAction)
    addSeparator()
    addAction(aboutAction)
    addAction(aboutQtAction)
  }

  private val mainToolBar = new QToolBar(this) {
    setFloatable(true)
    setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextUnderIcon)
    addAction(openImageAction)
  }
  
  private val analysisToolBar = new QToolBar(this) {
    setFloatable(true)
    setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextUnderIcon)
    addAction(analyseAction)
    addAction(configureAnalysersAction)
  }

  private val interactToolBar = new QToolBar(this) {
    setFloatable(true)
    setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextUnderIcon)
    addAction(zoomInAction)
    addAction(zoomOutAction)
    addSeparator()
    addAction(zoomOrigAction)
  }

  private val menu = new QMenuBar(this) {
    addMenu(menuFile)
    addMenu(menuWindow)
    addMenu(menuHelp)
  }

  addToolBar(mainToolBar)
  addToolBar(analysisToolBar)
  addToolBar(interactToolBar)

  setCentralWidget(tabWidget)
  setMenuBar(menu)

  private val fileDiag = new QFileDialog(this) {
    setFileMode(QFileDialog.FileMode.ExistingFile)
    setNameFilter(ImageIO.getReaderFileSuffixes.mkString("Image files (*.", " *.", ")"))
    setDirectory(QDir.homePath)
    this.setWindowTitle("Select image file")
  }

  def selectImage() = if(fileDiag.exec() != 0) {
    val filename = fileDiag.selectedFiles.get(0)
    val sessionWidget = new ImageSessionWidget(new ImageSession(new File(filename)))
    tabWidget.addTab(sessionWidget, new QIcon("classpath:icons/image-x-generic.png"), sessionWidget.windowTitle)
    updateToolbar()
  }

  private def changeSession(sessionNr: Int) = {
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

  private def updateToolbar() {
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
    AnalysisView.show()
    val imgWidget = tabWidget.currentWidget.asInstanceOf[ImageSessionWidget]
    imgWidget.imageSession.analyse()
    imgWidget.imageSession.analysis.foreach(_.finished.connect(this, "refreshResultsView()"))
    AnalysisView.analysis = imgWidget.imageSession.analysis
    updateToolbar()
  }

  private def refreshResultsView() = {
    ResultsView.show()
    val imgWidget = tabWidget.currentWidget.asInstanceOf[ImageSessionWidget]
    imgWidget.imageSession.analysis.foreach {a =>
      ResultsView.results = a.results
      a.finished.disconnect(this)
    }
  }

  def configureAnalysers() =
    tabWidget.currentWidget.asInstanceOf[ImageSessionWidget].configureAnalysers()
  
  def showAboutDialog() =
    QMessageBox.about(this, "About Purview", {
        <div>
          <h3>About Purview</h3>
          <p>
            Purview is an automated image forensics tool, used for detecting digital image forgeries.
          </p>
          <p>
            For more information, access to the source code and information
            about the project in general, please see
            <a href="http://github.com/dflemstr/purview">http://github.com/dflemstr/purview</a>
          </p>
          <p>
            Purview is available under the Apache 2.0 license.
            Please visit
            <a href="http://www.apache.org/licenses/">http://www.apache.org/licenses/</a>
            for more information.
          </p>
          <p>
            Copyright &copy; 2010 <em>David Flemström</em> and <em>Moritz Roth</em>
          </p>
        </div>
      }.toString
    )

  def showAboutQtDialog() = QMessageBox.aboutQt(this)

  private def analyserInfoClicked(obj: QObject) = {
    val analyser = obj.asInstanceOf[QAction].data.asInstanceOf[Analyser[ImageMatrix]]

    val mbText = {
      <table>
        <tr>
          <td><em>Name:</em></td>
          <td>{analyser.name}</td>
        </tr>
        <tr>
          <td><em>Description:</em></td>
          <td>{analyser.description}</td>
        </tr>
        {
          analyser.author.map {auth =>
            <tr>
              <td><em>Author:</em></td>
              <td>{auth}</td>
            </tr>
          } getOrElse ""
        }
        {
          analyser.version.map {ver =>
            <tr>
              <td><em>Version:</em></td>
              <td>{ver}</td>
            </tr>
          } getOrElse ""
        }
      </table>
    }.toString
    val buttons = new QMessageBox.StandardButtons(QMessageBox.StandardButton.Ok)
    val messageBox = new QMessageBox(QMessageBox.Icon.NoIcon, "About " + analyser.name, mbText, buttons, this)
    messageBox.setIconPixmap(new QPixmap("classpath:" + (analyser.iconResource getOrElse "icons/system-run.png"))
                             .scaled(64, 64, Qt.AspectRatioMode.KeepAspectRatio, Qt.TransformationMode.SmoothTransformation))
    messageBox.exec()
  }
}
