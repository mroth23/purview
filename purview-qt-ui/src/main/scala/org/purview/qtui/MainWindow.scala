package org.purview.qtui

import com.trolltech.qt.core.QDir
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

  setWindowTitle("Purview 1.2")
  setWindowIcon(QIcon.fromTheme("purview", new QIcon("classpath:icons/purview.png")))
  setMinimumSize(800, 600)

  addDockWidget(Qt.DockWidgetArea.BottomDockWidgetArea, AnalysisView)
  AnalysisView.hide()
  addDockWidget(Qt.DockWidgetArea.LeftDockWidgetArea, ResultsView)
  ResultsView.hide()
  addDockWidget(Qt.DockWidgetArea.BottomDockWidgetArea, LogView)
  LogView.hide()

  //A simple helper method for modifying something when it's constructed
  private def construct[A](what: A)(constructionBody: A => Any) = {
    constructionBody(what)
    what
  }

  private val shallowAnalysers = SessionUtils.createAnalyserInstances[ImageMatrix]

  private val tabWidget = new QTabWidget(this) {
    currentChanged.connect(this, "changeSession(int)")
    tabCloseRequested.connect(this, "closeTab(int)")
    tabCloseRequested.connect(MainWindow.this, "updateToolbar()")
    setDocumentMode(true)
    setTabsClosable(true)

    private def changeSession(sessionNr: Int) = {
      val ana = Option(currentWidget.asInstanceOf[ImageSessionWidget]) flatMap {_.imageSession.analysis}
      AnalysisView.analysis = ana
      ResultsView.analysis = ana
      updateToolbar()
    }

    def closeTab(tab: Int) = {
      widget(tab).close()
      removeTab(tab)
    }
  }

  private val openImageAction = new QAction(this) {
    setText("&Open Image...")
    setShortcut("Ctrl+N")
    setIcon(QIcon.fromTheme("folder-image", new QIcon("classpath:icons/folder-image.png")))
    triggered.connect(this, "selectImage()")

    private def selectImage() = {
      val filename = QFileDialog.getSaveFileName(MainWindow.this, "Open image", QDir.homePath,
                                                 new QFileDialog.Filter(ImageIO.getReaderFileSuffixes.mkString("Image files (*.", " *.", ")")))
      val sessionWidget = new ImageSessionWidget(new ImageSession(new File(filename)))
      tabWidget.addTab(sessionWidget, QIcon.fromTheme("image-x-generic", new QIcon("classpath:icons/image-x-generic.png")), sessionWidget.windowTitle)
      updateToolbar()
    }
  }

  private val saveReportAction = new QAction(this) {
    setText("Save report image...")
    setShortcut("Ctrl+S")
    setIcon(QIcon.fromTheme("document-save-all", new QIcon("classpath:icons/document-save-all.png")))
    triggered.connect(this, "saveImages()")

    def saveImages() = {
      val file = QFileDialog.getSaveFileName(MainWindow.this, "Save SVG", QDir.homePath, new QFileDialog.Filter("SVG files (*.svg)"))
      Option(tabWidget.currentWidget.asInstanceOf[ImageSessionWidget]).foreach(_.saveImageTo(file))
    }
  }

  private val exitAction = new QAction(this) {
    setText("&Quit Purview")
    setShortcut("Ctrl+Q")
    setIcon(QIcon.fromTheme("dialog-error", new QIcon("classpath:icons/dialog-error.png")))
    triggered.connect(QApplication.instance(), "quit()")
  }

  private val showAnalysisAction = new QAction(this) {
    setText("&Analysis window")
    setShortcut("Ctrl+A")
    setIcon(AnalysisView.windowIcon)
    setCheckable(true)
    setChecked(false)
    toggled.connect(AnalysisView, "setVisible(boolean)")
    AnalysisView.visibilityChanged.connect(this: QAction /*!!*/, "setChecked(boolean)")
  }

  private val showResultsAction = new QAction(this) {
    setText("&Results window")
    setShortcut("Ctrl+R")
    setIcon(ResultsView.windowIcon)
    setCheckable(true)
    setChecked(false)
    toggled.connect(ResultsView, "setVisible(boolean)")
    ResultsView.visibilityChanged.connect(this: QAction /*!!*/, "setChecked(boolean)")
  }

  private val showLogAction = new QAction(this) {
    setText("&Log window")
    setShortcut("Ctrl+L")
    setIcon(LogView.windowIcon)
    setCheckable(true)
    setChecked(false)
    toggled.connect(LogView, "setVisible(boolean)")
    LogView.visibilityChanged.connect(this: QAction /*!!*/, "setChecked(boolean)")
  }

  private val aboutAction = new QAction(this) {
    setText("&About Purview...")
    setIcon(QIcon.fromTheme("dialog-information", new QIcon("classpath:icons/dialog-information.png")))
    setShortcut("F1")
    triggered.connect(this, "showAboutDialog()")
    private def showAboutDialog() = QMessageBox.about(MainWindow.this, "About Purview", MainWindowTemplates.aboutText)
  }

  private val aboutQtAction = new QAction(this) {
    setText("About &Qt...")
    setIcon(QIcon.fromTheme("qt", new QIcon("classpath:icons/qt.png")))
    triggered.connect(this, "showAboutQtDialog()")
    private def showAboutQtDialog() = QMessageBox.aboutQt(MainWindow.this)
  }

  private val analyseAction = new QAction(this) {
    setText("Analyse &image")
    setIcon(QIcon.fromTheme("system-run", new QIcon("classpath:icons/system-run.png")))
    setShortcut("Ctrl+A")
    setEnabled(false)
    triggered.connect(this, "analyse()")
    private def analyse() = {
      AnalysisView.show()
      val imgWidget = tabWidget.currentWidget.asInstanceOf[ImageSessionWidget]
      imgWidget.analyse()
      imgWidget.imageSession.analysis.foreach(_.finished.connect(this, "refreshResultsView()"))
      AnalysisView.analysis = imgWidget.imageSession.analysis
      updateToolbar()
    }
    private def refreshResultsView() = {
      ResultsView.show()
      val imgWidget = Option(tabWidget.currentWidget.asInstanceOf[ImageSessionWidget])
      ResultsView.analysis = imgWidget.flatMap(_.imageSession.analysis)
      ResultsView.analysis.foreach {a =>
        a.finished.disconnect(this)
      }
    }
  }

  private val configureAnalysersAction = new QAction(this) {
    setText("&Configure analysers...")
    setIcon(QIcon.fromTheme("configure", new QIcon("classpath:icons/configure.png")))
    setShortcut("Ctrl+C")
    setEnabled(false)
    triggered.connect(this, "configureAnalysers()")
    private def configureAnalysers() = tabWidget.currentWidget.asInstanceOf[ImageSessionWidget].configureAnalysers()
  }

  private val zoomInAction = new QAction(this) {
    setText("Zoom &in")
    setShortcut("Ctrl++")
    setIcon(QIcon.fromTheme("zoom-in", new QIcon("classpath:icons/zoom-in.png")))
    setEnabled(false)
    triggered.connect(this, "zoomIn()")
    def zoomIn() = tabWidget.currentWidget.asInstanceOf[ImageSessionWidget].scale(1.25, 1.25)
  }

  private val zoomOutAction = new QAction(this) {
    setText("Zoom &out")
    setShortcut("Ctrl+-")
    setIcon(QIcon.fromTheme("zoom-out", new QIcon("classpath:icons/zoom-out.png")))
    setEnabled(false)
    triggered.connect(this, "zoomOut()")
    private def zoomOut() = tabWidget.currentWidget.asInstanceOf[ImageSessionWidget].scale(0.8, 0.8)
  }

  private val zoomOrigAction = new QAction(this) {
    setText("O&riginal size")
    setShortcut("Ctrl+0")
    setIcon(QIcon.fromTheme("zoom-original", new QIcon("classpath:icons/zoom-original.png")))
    setEnabled(false)
    triggered.connect(this, "zoomOrig()")
    private def zoomOrig() = tabWidget.currentWidget.asInstanceOf[ImageSessionWidget].resetTransform()
  }

  private val analyserActions = for(analyser <- shallowAnalysers) yield
    new QAction(this) {
      setIcon(analyser.iconResource.map(x => new QIcon("classpath:" + x)) getOrElse QIcon.fromTheme("system-run", new QIcon("classpath:icons/system-run.png")))
      setText("About \"" + analyser.name + "\"...")
      setToolTip(analyser.description)
      setData(analyser)
      triggered.connect(this, "analyserInfoClicked()")
      private def analyserInfoClicked() = {
        val buttons = new QMessageBox.StandardButtons(QMessageBox.StandardButton.Ok)
        val messageBox = new QMessageBox(QMessageBox.Icon.NoIcon, "About \"" + analyser.name + "\"",
                                         MainWindowTemplates.aboutAnalyserText(analyser), buttons, MainWindow.this)
        val pixmap = new QPixmap("classpath:" + (analyser.iconResource getOrElse "icons/system-run.png"))
        val scaledPixmap = pixmap.scaled(64, 64,
                                         Qt.AspectRatioMode.KeepAspectRatio,
                                         Qt.TransformationMode.SmoothTransformation)
        messageBox.setIconPixmap(scaledPixmap)
        messageBox.exec()
      }
    }

  private val menuFile = new QMenu(this) {
    setTitle("&File")
    addAction(openImageAction)
    addAction(saveReportAction)
    addAction(analyseAction)
    addAction(configureAnalysersAction)
    addSeparator()
    addAction(exitAction)
  }

  private val menuWindow = new QMenu(this) {
    setTitle("&Window")
    addAction(showAnalysisAction)
    addAction(showResultsAction)
    addAction(showLogAction)
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
    setWindowTitle("&Main toolbar")
    setFloatable(true)
    setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextBesideIcon)
    addAction(openImageAction)
  }

  private val analysisToolBar = new QToolBar(this) {
    setWindowTitle("A&nalysis toolbar")
    setFloatable(true)
    setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextBesideIcon)
    addAction(analyseAction)
    addAction(configureAnalysersAction)
  }

  private val interactToolBar = new QToolBar(this) {
    setWindowTitle("Interact tool&bar")
    setFloatable(true)
    setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextBesideIcon)
    addAction(zoomInAction)
    addAction(zoomOutAction)
    addSeparator()
    addAction(zoomOrigAction)
  }

  private val menu = new QMenuBar(this) {
    setWindowTitle("Menu bar")
    addMenu(menuFile)
    addMenu(menuWindow)
    addMenu(menuHelp)
  }

  addToolBar(mainToolBar)
  addToolBar(analysisToolBar)
  addToolBar(interactToolBar)

  setCentralWidget(tabWidget)
  setMenuBar(menu)

  protected def updateToolbar() {
    val enabled = (tabWidget.currentIndex > -1)
    val imgWidget = Option(tabWidget.currentWidget.asInstanceOf[ImageSessionWidget])
    analyseAction.setEnabled(imgWidget flatMap (_.imageSession.analysis) match {
        case Some(analysis) => analysis.results.isDefined
        case None => enabled
      })
    configureAnalysersAction.setEnabled(enabled)
    zoomInAction.setEnabled(enabled)
    zoomOutAction.setEnabled(enabled)
    zoomOrigAction.setEnabled(enabled)
  }
}

object MainWindowTemplates {
  val aboutText = {
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
  
  def aboutAnalyserText(analyser: Analyser[ImageMatrix]) = {
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
}
