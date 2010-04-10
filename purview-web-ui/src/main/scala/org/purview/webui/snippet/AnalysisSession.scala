package org.purview.webui.snippet

import java.io.StringWriter
import net.liftweb.common.Full
import net.liftweb.common.Logger
import net.liftweb.http.DispatchSnippet
import net.liftweb.http.FileParamHolder
import net.liftweb.http.RequestVar
import net.liftweb.http.S
import net.liftweb.http.SHtml
import net.liftweb.http.SessionVar
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import net.liftweb.util.Helpers
import net.liftweb.util.Helpers._
import org.apache.batik.dom.GenericDOMImplementation
import org.apache.batik.svggen.SVGGeneratorContext
import org.apache.batik.svggen.SVGGraphics2D
import org.purview.core.analysis.Analyser
import org.purview.core.analysis.Metadata
import org.purview.core.analysis.Settings
import org.purview.core.analysis.settings.FloatRangeSetting
import org.purview.core.analysis.settings.IntRangeSetting
import org.purview.core.analysis.settings.Setting
import org.purview.core.data.ImageMatrix
import org.purview.core.report.Message
import org.purview.core.report.ReportEntry
import org.purview.core.session.SessionUtils
import org.purview.webui.util.Analysers
import org.purview.webui.util.Analysis
import org.purview.webui.util.AnalysisActor
import org.purview.webui.util.AnalysisActor
import org.purview.webui.util.AnalysisRuntime
import org.purview.webui.util.ImageUtils
import org.purview.webui.util.ReportEntryRenderer
import org.purview.webui.util.ReportManager
import org.purview.webui.util.SVGImageHandler
import scala.xml.NodeSeq
import scala.xml.Text
import scala.xml.XML

object AnalysisSession {
  object analyses extends SessionVar[Map[String, Analysis]](Map.empty)
}

class AnalysisSession extends DispatchSnippet with Logger {
  import AnalysisSession._

  def dispatch = {
    case "create" => create
    case "image" => image
    case "imageName" => imageName
    case "analyserList" => analyserList
    case "resultsView" => resultsView
  }

  private object uploadedFile extends RequestVar[Option[FileParamHolder]](None)
  def create(createTemplate: NodeSeq) = {
    def doCreate() = uploadedFile.is match {
      case None =>
        S.error("No file was uploaded")
      case Some(fileParam) =>
        val inputImage = try {
          ImageUtils.createInputImage(fileParam.fileName, fileParam.fileStream)
        } catch {
          case ex =>
            S.error("Couldn't open the uploaded file")
            info(ex.getStackTraceString)
            S.redirectTo("/image")
        }

        if(inputImage.original.file.length() > 10 * 1024 * 1024)
          S.warning("You uploaded a large image (> 10 MiB). This might lead to a slow analysis.")

        val analysers = try {
          SessionUtils.createAnalyserInstances[ImageMatrix]()
        } catch {
          case ex =>
            S.error("Error when initializing an analyser (check the server logs for more information)")
            info(ex.getStackTraceString)
            S.redirectTo("/image")
        }

        val analysisId = {
          val baseName = fileParam.fileName.replaceAll("[^a-zA-Z0-9_]", "_")
          var res = baseName
          var i = 0
          while(analyses.is.contains(res)) {
            i += 1
            res = baseName + "_" + i
          }
          res
        }

        val analysis = Analysis(
          inputImage = inputImage,
          analysers = Analysers(
            instances = analysers,
            enabled = analysers.map((_, false)).toMap
          ),
          runtime = None
        )

        analyses.set(analyses.is + (analysisId -> analysis))

        S.notice("The image was successfully uploaded!")
        S.redirectTo("/analysers/" + analysisId)
    }

    bind("create", createTemplate,
         "image" -> SHtml.fileUpload(x => uploadedFile.set(Some(x))),
         "submit" -> SHtml.submit("Start session", doCreate))
  }

  def imageName(otherwise: NodeSeq) =
    analyses.is.get(S.param("analysisId") openOr "") map { x =>
      Text(x.inputImage.name)
    } getOrElse otherwise

  def image(otherwise: NodeSeq) =
    analyses.is.get(S.param("analysisId") openOr "") map { x =>
      (<img src={S.hostAndPath + "/imagefile/" + x.inputImage.scaled.id} alt={x.inputImage.name}/>)
    } getOrElse otherwise

  def analyserList(analyserListTemplate: NodeSeq) = {
    val analysisId = S.param("analysisId") openOr ""
    def analysis = analyses.is.getOrElse(analysisId, {
        S.error("Can't view analysers: No active analysis")
        S.redirectTo("/")
      })
    def makeEntry(entryTemplate: NodeSeq): NodeSeq =
      analysis.analysers.instances.flatMap { analyser =>

        def doSetEnabled(value: Boolean) = {
          val newAnalysers = analysis.analysers.copy(enabled = analysis.analysers.enabled + (analyser -> value))
          val newAnalysis = analysis.copy(analysers = newAnalysers)
          analyses.set(analyses.is + (analysisId -> newAnalysis))
        }

        val analyserEnabled = analysis.analysers.enabled(analyser)

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
      val elemId = randomString(16)
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
      }

      def redraw(): JsCmd = SetHtml(elemId, makeSetting())
      <span id={elemId}>{makeSetting()}</span>
    }

    def doSubmit() = {
      import org.purview.core

      val analysisActor = new AnalysisActor
      analysisActor.start()
      val runtime = AnalysisRuntime(
        running = true,
        resultsKey = ReportManager.makeId,
        analysisActor
      )

      val mgImage = analysis.inputImage.original
      val image = try {
        ImageMatrix.fromFile(mgImage.file)
      } catch {
        case _ =>
          S.error("Cannot analyse image, The uploaded file has ceased to exist!") //Who knows...
          S.redirectTo("/image")
      }

      analyses.set(analyses.is + (analysisId -> analysis.copy(runtime = Some(runtime))))

      val session = new core.session.AnalysisSession[ImageMatrix](analysis.analysers.instances.filter(analysis.analysers.enabled), image)
      val thread = new Thread(new Runnable {
          def run() = {
            try {
              ReportManager.saveReport(session.run(analysisActor), runtime.resultsKey)
            } catch {
              case ex => analysisActor.error(ex.getMessage)
            }
            analysisActor.done()
          }
        })
      thread.start()
      S.redirectTo("/process/" + analysisId)
    }

    bind("analyserList", analyserListTemplate,
         "entry" -> makeEntry _,
         "submit" -> SHtml.submit("Start analysis", doSubmit))
  }

  def resultsView(resultsViewTemplate: NodeSeq): NodeSeq = {
    val elemId = randomString(16)
    val analysisId = S.param("analysisId") openOr ""
    def analysis = analyses.is.getOrElse(analysisId, {
        S.error("Can't view any results; no active analysis")
        S.redirectTo("/")
      })
    val image = analysis.inputImage.original.load()

    val report = ReportManager.loadReport(analysis.runtime.map(_.resultsKey) getOrElse "")
    val res = report.map(x => x._1 -> x._2.toSeq.sortWith((x, y) => x.level.name < y.level.name))

    val metas = res.keySet.toSeq.sortWith((x, y) => x.name < y.name)

    var currentReportEntry: Option[ReportEntry] = None

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

    def makeReportEntry(metadata: Metadata)(reportEntryTemplate: NodeSeq): NodeSeq =
      res(metadata).flatMap { entry =>
        val isCurrent = currentReportEntry.map(_ == entry) getOrElse false
        val label = entry match {
          case m: Message => Text(m.message)
          case _ => Text(entry.toString)
        }

        bind("reportEntry", reportEntryTemplate,
             "message" -> SHtml.a(() => {currentReportEntry = Some(entry); redraw}, label))
      }.toList

    def makeView(viewTemplate: NodeSeq): NodeSeq = currentReportEntry match {
      case None =>
        (<img src={S.hostAndPath + "/imagefile/" + analysis.inputImage.original.id} alt={analysis.inputImage.name}/>)
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

    def redraw() = SetHtml(elemId, inner())

    def inner() = {
      bind("resultsView", resultsViewTemplate,
           "tree" -> makeTree _,
           "view" -> makeView _)
    }
    <div id={elemId} style="padding: 0.1em;">{inner()}</div>
  }
}
