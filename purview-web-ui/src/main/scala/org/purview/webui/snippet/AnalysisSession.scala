package org.purview.webui.snippet

import java.io.StringWriter
import net.liftweb.common.Full
import net.liftweb.common.Logger
import net.liftweb.http.DispatchSnippet
import net.liftweb.http.FileParamHolder
import net.liftweb.http.RequestVar
import net.liftweb.http.S
import net.liftweb.http.SHtml
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds
import net.liftweb.http.js.JsCmds._
import net.liftweb.util.Helpers
import net.liftweb.util.Helpers._
import org.apache.batik.dom.GenericDOMImplementation
import org.apache.batik.svggen.SVGGeneratorContext
import org.apache.batik.svggen.SVGGraphics2D
import org.purview.core.analysis.Analyser
import org.purview.core.analysis.Settings
import org.purview.core.analysis.Metadata
import org.purview.core.analysis.settings.BooleanSetting
import org.purview.core.analysis.settings.FloatRangeSetting
import org.purview.core.analysis.settings.IntRangeSetting
import org.purview.core.analysis.settings.Setting
import org.purview.core.data.ImageMatrix
import org.purview.core.report.ReportEntry
import org.purview.core.session.SessionUtils
import org.purview.webui.db.Analysis
import org.purview.webui.db.Database
import org.purview.webui.util.AnalysisActor
import org.purview.webui.util.ImageManager
import org.purview.webui.util.ImageUtils
import org.purview.webui.util.ReportManager
import org.purview.webui.util.ReportEntryRenderer
import org.purview.webui.util.SVGImageHandler
import org.purview.webui.util.UploadManager
import org.squeryl.PrimitiveTypeMode
import org.squeryl.PrimitiveTypeMode._
import scala.collection.mutable
import scala.xml.NodeSeq
import scala.xml.Text
import scala.xml.XML

object AnalysisSession {
  @volatile var stalledAnalyses: Map[Long, Map[Analyser[ImageMatrix], Boolean]] = Map.empty
  @volatile var runningAnalyses: Map[Long, AnalysisActor] = Map.empty
  @volatile var completedAnalyses: List[Long] = Nil

  val MaxCompletedAnalyses = 10
}

class AnalysisSession extends DispatchSnippet with Logger {
  import AnalysisSession._

  def dispatch = {
    case "create" => create
    case "image" => image
    case "imageName" => imageName
    case "analyserList" => analyserList
    case "resultsView" => resultsView
    case "resultsTree" => resultsTree
    case "runningAnalyses" => runningAnalyses
    case "completedAnalyses" => completedAnalyses
  }

  private object uploadedFile extends RequestVar[Option[FileParamHolder]](None)
  def create(createTemplate: NodeSeq) = {
    def doCreate() = uploadedFile.is match {
      case None =>
        S.error("No file was uploaded")
      case Some(fileParam) => transaction {
          val handle = {
            val base = fileParam.fileName.replaceAll("""[^\w_-]""", "_")
            var res = base
            var i = 0
            while(from(Database.analyses)(analysis => where(analysis.handle === res) compute(count)) > 0l) {
              i += 1
              res = base + "_" + i
            }
            res
          }
          
          try ImageUtils.makeImageSet(fileParam.fileStream, handle) catch {
            case ex =>
              S.error("Couldn't open the uploaded file")
              info(ex.getStackTraceString)
              S.redirectTo(S.referer openOr "/")
          }

          val analysers = try SessionUtils.createAnalyserInstances[ImageMatrix]() catch {
            case ex =>
              S.error("Error when initializing an analyser (check the server logs for more information)")
              info(ex.getStackTraceString)
              S.redirectTo(S.referer openOr "/")
          }

          val analysis = Database.analyses.insert(Analysis(
              fileName = fileParam.fileName,
              handle = handle
            ))
          stalledAnalyses = stalledAnalyses.updated(analysis.id, analysers.map((_, true)).toMap)

          S.notice("The image was successfully uploaded!")
          S.redirectTo("/analysis/" + handle + "/configure_analysers")
        }
    }

    bind("create", createTemplate,
         "image" -> <span id="image-field">{SHtml.fileUpload(x => uploadedFile.set(Some(x)))}</span>,
         "submit" -> SHtml.submit("Start session", doCreate))
  }

  def currentAnalysisHandle = S.param("analysisId").toOption
  
  def currentAnalysis = transaction {
    currentAnalysisHandle.flatMap(handle => from(Database.analyses)(analysis => where(analysis.handle === handle) select(analysis)).headOption)
  }

  def runningAnalyses(runningAnalysesTemplate: NodeSeq) = transaction {
    def makeEntry(entryTemplate: NodeSeq): NodeSeq = {
      val result = (for {
          analysisId <- AnalysisSession.runningAnalyses.keySet.toSeq.sortWith(_ < _)
          analysis <- from(Database.analyses)(analysis => where(analysis.id === analysisId) select(analysis)).headOption
        } yield bind("entry", entryTemplate,
                     "link" -> <a href={S.hostAndPath + "/analysis/" + analysis.handle + "/progress"}>{analysis.fileName}</a>)).flatten
      if(result.isEmpty) bind("entry", entryTemplate, "link" -> Text("No currently running analyses")) else result
    }
    bind("runningAnalyses", runningAnalysesTemplate,
         "entry" -> makeEntry _)
  }

  def completedAnalyses(completedAnalysesTemplate: NodeSeq) = transaction {
    def makeEntry(entryTemplate: NodeSeq): NodeSeq = {
      val result = (for {
          analysisId <- AnalysisSession.completedAnalyses
          analysis <- from(Database.analyses)(analysis => where(analysis.id === analysisId) select(analysis)).headOption
        } yield bind("entry", entryTemplate,
                     "link" -> <a href={S.hostAndPath + "/analysis/" + analysis.handle + "/results"}>{analysis.fileName}</a>)).flatten
      if(result.isEmpty) bind("entry", entryTemplate, "link" -> Text("No completed analyses yet")) else result
    }
    bind("completedAnalyses", completedAnalysesTemplate,
         "entry" -> makeEntry _)
  }

  def imageName(otherwise: NodeSeq) =
    currentAnalysis map (_.fileName) map (Text) getOrElse otherwise

  def image(otherwise: NodeSeq) =
    currentAnalysis map { analysis =>
      (<img src={S.hostAndPath + "/image/" + analysis.handle + "-scaled.png"} alt={analysis.fileName}/>) % S.attrsToMetaData
    } getOrElse otherwise

  def analyserList(analyserListTemplate: NodeSeq) = {
    val analysis = currentAnalysis getOrElse {
      S.error("Can't view analysers: No active analysis")
      S.redirectTo(S.referer openOr "/")
    }
    val analysers = stalledAnalyses.getOrElse(analysis.id, {
        S.error("No loaded analysers for analysis")
        S.redirectTo(S.referer openOr "/")
      })
    def makeEntry(entryTemplate: NodeSeq): NodeSeq =
      analysers.keySet.toSeq.sortBy(_.name).flatMap { analyser =>

        def doSetEnabled(value: Boolean) =
          stalledAnalyses = stalledAnalyses.updated(analysis.id, stalledAnalyses(analysis.id).updated(analyser, value))

        val analyserEnabled = stalledAnalyses(analysis.id)(analyser)

        bind("entry", entryTemplate,
             "enabled" -> SHtml.checkbox(analyserEnabled, doSetEnabled),
             "name" -> analyser.name,
             "description" -> analyser.description,
             "settings" -> makeSettings(analyser) _)
      }

    def makeSettings(analyser: Analyser[ImageMatrix])(settingsTemplate: NodeSeq): NodeSeq = analyser match {
      case settings: Settings =>
        bind("settings", settingsTemplate,
             "setting" -> makeSetting(settings.settings) _)
      case _ =>
        NodeSeq.Empty
    }

    def makeSetting(settings: Seq[Setting[_]])(settingTemplate: NodeSeq): NodeSeq =
      settings.flatMap { setting =>
        bind("setting", settingTemplate,
             "name" -> setting.name,
             "form" -> settingForm(setting))
      }

    def settingForm(s: Setting[_]) = {
      val elemId = nextFuncName
      def makeSetting() = s match {
        case intRange: IntRangeSetting =>
          def doChangeValue(newVal: String) = try {
            newVal.toInt match {
              case x if x >= intRange.min && x <= intRange.max =>
                intRange.value = x
              case x if x < intRange.min =>
                S.error("Value too small, minimum is " + intRange.min)
              case x if x > intRange.max =>
                S.error("Value too large, maximum is " + intRange.max)
            }
          } catch {
            case _ => S.error("Invalid integer number")
          }
          SHtml.swappable(<span>{intRange.value.toString}</span>,
                          SHtml.ajaxSelect((intRange.min to intRange.max).map(x => (x.toString, x.toString)),
                                           Full(intRange.value.toString), x => {doChangeValue(x); redraw()}))
        case floatRange: FloatRangeSetting =>
          def doChangeValue(newVal: String) = try {
            newVal.replace(",", ".").toFloat match {
              case x if x >= floatRange.min && x <= floatRange.max =>
                floatRange.value = x
              case x if x < floatRange.min =>
                S.error("Value too small, minimum is " + floatRange.min)
              case x if x > floatRange.max =>
                S.error("Value too large, maximum is " + floatRange.max)
            }
          } catch {
            case _ => S.error("Invalid floating point number")
          }
          SHtml.swappable(<span>{floatRange.value.toString}</span>,
                          SHtml.ajaxSelect((floatRange.min to floatRange.max by 1f/floatRange.granularity)
                                           .map(x => (x.toString, x.toString)),
                                           Full(floatRange.value.toString), x => {doChangeValue(x); redraw()}))
        case boolean: BooleanSetting =>
          SHtml.swappable(if(boolean.value) <span class="enabled"/> else <span class="disabled"/>,
                          SHtml.ajaxCheckbox(boolean.value, x => {boolean.value = x; redraw() }))
      }

      def redraw(): JsCmd = SetHtml(elemId, makeSetting())
      <span id={elemId}>{makeSetting()}</span>
    }

    def doSubmit() = {
      import org.purview.core

      val image = try ImageMatrix.fromFile(UploadManager.file(analysis.handle)) catch {
        case _ =>
          S.error("Cannot analyse image, the uploaded file has ceased to exist!") //Who knows...
          S.redirectTo(S.referer openOr "/")
      }
      
      val analysisActor = new AnalysisActor
      analysisActor.start()
      AnalysisSession.runningAnalyses = AnalysisSession.runningAnalyses.updated(analysis.id, analysisActor)

      val analyserCandidates = stalledAnalyses(analysis.id).keySet.toSeq.sortBy(_.name)
      val session = new core.session.AnalysisSession[ImageMatrix](analyserCandidates.filter(stalledAnalyses(analysis.id)), image)
      val thread = new Thread(new Runnable {
          def run() = {
            try {
              val results = session.run(analysisActor)
              ReportManager.write(analysis.handle, results)
              AnalysisSession.completedAnalyses = analysis.id :: AnalysisSession.completedAnalyses.take(MaxCompletedAnalyses - 1)
            } catch {
              case ex => 
                error(ex + "\n" + ex.getMessage + "\n" + ex.getStackTraceString)
                analysisActor.error(ex.getMessage)
            }
            analysisActor.done()
          }
        })
      thread.start()

      AnalysisSession.stalledAnalyses -= analysis.id

      S.redirectTo("/analysis/" + analysis.handle + "/progress")
    }

    bind("analyserList", analyserListTemplate,
         "entry" -> makeEntry _,
         "submit" -> SHtml.submit("Start analysis", doSubmit))
  }

  object currentReportEntry extends RequestVar[Option[ReportEntry]](None)
  object resultsElemId extends RequestVar[String](nextFuncName)
  object redrawFunc extends RequestVar[() => JsCmd](() => JsCmds.Noop)

  def resultsTree(resultsTreeTemplate: NodeSeq): NodeSeq = {
    val analysis = currentAnalysis getOrElse {
      S.error("Can't view result: No active analysis")
      S.redirectTo(S.referer openOr "/")
    }
    val result = ReportManager.read(analysis.handle) getOrElse {
      S.error("Can't view result: Results for analysis don't exist")
      S.redirectTo(S.referer openOr "/")
    }
    val metas = result.keySet.toSeq.sortBy(_.name)

    def makeTree(treeTemplate: NodeSeq): NodeSeq = {
      bind("tree", treeTemplate,
           "analyserEntry" -> makeAnalyserEntry _)
    }

    def makeAnalyserEntry(analyserEntryTemplate: NodeSeq): NodeSeq =
      metas.flatMap { metadata =>
        bind("analyserEntry", analyserEntryTemplate,
             "analyserName" -> metadata.name,
             "analyserDescription" -> metadata.description,
             "reportEntry" -> makeReportEntry(metadata) _)
      }.toList

    def makeReportEntry(report: Metadata)(reportEntryTemplate: NodeSeq): NodeSeq =
      result(report).toSeq.sortBy(_.level.name).flatMap { entry =>
        val label = Text(entry.message)

        bind("reportEntry", reportEntryTemplate,
             "message" -> SHtml.a(() => {currentReportEntry.set(Some(entry)); redrawFunc.is()}, label,
                                  "class" -> ("report-level-" + entry.level.name)))
      }.toList
    makeTree(resultsTreeTemplate)
  }

  def resultsView(resultsViewTemplate: NodeSeq): NodeSeq = {
    val analysis = currentAnalysis getOrElse {
      S.error("Can't view result: No active analysis")
      S.redirectTo(S.referer openOr "/")
    }
    val image = ImageManager.read(analysis.handle + "-optimized") getOrElse {
      S.error("Can't view result: source image not found")
      S.redirectTo(S.referer openOr "/")
    }

    //Unload analysers
    if(AnalysisSession.runningAnalyses.contains(analysis.id))
      AnalysisSession.runningAnalyses = AnalysisSession.runningAnalyses - analysis.id

    def makeView(viewTemplate: NodeSeq): NodeSeq = currentReportEntry.is match {
      case None =>
        (<img src={S.hostAndPath + "/image/" + analysis.handle + "-optimized.png"} alt={analysis.fileName}/>)
      case Some(entry) =>
        val domImpl = GenericDOMImplementation.getDOMImplementation
        val svgNS = "http://www.w3.org/2000/svg"
        val document = domImpl.createDocument(svgNS, "svg", null)

        val context = SVGGeneratorContext.createDefault(document)
        context.setImageHandler(SVGImageHandler)

        val g = new SVGGraphics2D(context, false)
        try {
          g.drawImage(image, 0, 0, null)
          ReportEntryRenderer.renderReportEntry(g, entry)
        } catch {
          case ex =>
            error(ex + "\n" + ex.getStackTraceString)
        }

        val out = new StringWriter
        g.stream(out, true)
        g.dispose()
        val svg = XML.loadString(out.toString)
        <div style={"width: " + image.getWidth + "px; height: " + image.getHeight + "px;"}>{svg}</div>
    }

    def inner() = makeView(resultsViewTemplate)

    redrawFunc.set(() => SetHtml(resultsElemId.is, inner()))
    <div id={resultsElemId.is}>{inner()}</div> % S.attrsToMetaData
  }
}
