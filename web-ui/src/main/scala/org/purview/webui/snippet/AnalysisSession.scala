package org.purview.webui.snippet

import java.awt.image.BufferedImage
import java.io.StringWriter
import net.liftweb.actor.LiftActor
import net.liftweb.common.Full
import net.liftweb.common.Logger
import net.liftweb.http.DispatchSnippet
import net.liftweb.http.FileParamHolder
import net.liftweb.http.RequestVar
import net.liftweb.http.S
import net.liftweb.http.SHtml
import net.liftweb.http.SessionVar
import net.liftweb.http.js.JsCmds._
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
import org.purview.webui.util.AnalysisActor
import org.purview.webui.util.ImageManager
import org.purview.webui.util.AnalysisActor
import org.purview.webui.util.ReportEntryRenderer
import org.purview.webui.util.ReportManager
import org.purview.webui.util.SVGImageHandler
import scala.actors.Actor
import scala.xml.NodeSeq
import scala.xml.Text
import scala.xml.XML
import scala.xml.dtd.DocType

object AnalysisSession {
  object uploadedFile extends RequestVar[Option[FileParamHolder]](None)
  object inputImage extends SessionVar[Option[ImageManager.Image]](None)
  object inputImageReduced extends SessionVar[Option[ImageManager.Image]](None)
  object inputImageName extends SessionVar[Option[String]](None)
  object analysers extends SessionVar[Seq[Analyser[ImageMatrix]]](Seq.empty)
  object activeAnalysers extends SessionVar[Map[Analyser[ImageMatrix], Boolean]](Map.empty)
  object running extends SessionVar[Boolean](false)
  object resultsKey extends SessionVar[String]("")
  object results extends SessionVar[Map[Metadata, Set[ReportEntry]]](Map.empty)

  object stats extends SessionVar[Option[AnalysisActor]](None)
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

  def create(createTemplate: NodeSeq) = {
    def doCreate() = uploadedFile.is match {
      case None =>
        S.error("No file was uploaded")
      case Some(fileParam) =>
        val stream = fileParam.fileStream
        val image = try {
          ImageManager.saveImage(stream)
        } catch {
          case ex =>
            S.error("Invalid image file")
            info(ex.getStackTraceString)
            S.redirectTo("/image")
        }

        val inImage = try {
          image.load()
        } catch {
          case ex =>
            S.error("Invalid image file")
            info(ex.getStackTraceString)
            S.redirectTo("/image")
        }
        
        info("Started a new image session for image " + fileParam.fileName)
        inputImageName.set(Some(fileParam.fileName))
        inputImage.set(Some(image))

        val stretchFactor = (750f / inImage.getWidth) min (562.5f / inImage.getHeight) min 1.0f
        val outImage = new BufferedImage((inImage.getWidth * stretchFactor).toInt,
                                         (inImage.getHeight * stretchFactor).toInt, BufferedImage.TYPE_INT_ARGB)
        val g = outImage.createGraphics
        try {
          g.drawImage(inImage, 0, 0, (inImage.getWidth * stretchFactor).toInt, (inImage.getHeight * stretchFactor).toInt, null)
        } finally {
          g.dispose()
        }
        inputImageReduced.set(Some(ImageManager.saveImage(outImage)))

        val analysers = try {
          SessionUtils.createAnalyserInstances()
        } catch {
          case ex =>
            S.error("Error when initializing analyser")
            info(ex.getStackTraceString)
            S.redirectTo("/")
        }
        AnalysisSession.analysers.set(analysers)
        activeAnalysers.set(analysers.map((_, false)).toMap)
        
        S.redirectTo("/analysers")
    }

    bind("create", createTemplate,
         "image" -> SHtml.fileUpload(x => uploadedFile.set(Some(x))),
         "submit" -> ((x: NodeSeq) => SHtml.submit(x.text, doCreate)))
  }

  def imageName(unused: NodeSeq) =
    inputImageName.map(Text(_)) getOrElse <em>unknown</em>

  def image(otherwise: NodeSeq) =
    inputImageReduced.is map (x => <img src={S.hostAndPath + "/imagefile/" + x.id}/>) getOrElse otherwise

  def analyserList(analyserListTemplate: NodeSeq) = {
    val id = randomString(16)
    def makeEntry(entryTemplate: NodeSeq): NodeSeq =
      analysers.flatMap { analyser =>
        val (name, descr) = analyser match {
          case meta: Metadata => (meta.name, meta.description)
          case _ => ("Nameless analyser", "Unknown")
        }

        def doSetEnabled(value: Boolean) = {
          activeAnalysers.set(activeAnalysers.is + (analyser -> value))
          redraw()
        }
        
        bind("entry", entryTemplate,
             "enabled" -> SHtml.ajaxCheckbox(activeAnalysers(analyser), doSetEnabled),
             "name" -> name,
             "description" -> descr,
             "settings" -> (if(activeAnalysers(analyser)) makeSettings(analyser) _ else (x: NodeSeq) => NodeSeq.Empty))
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

    def settingForm(s: Setting[_]) = s match {
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
                        SHtml.ajaxText(floatRange.value.toString, x => {doChangeValue(x); redraw()}))
    }
    
    def doSubmit() = {
      import org.purview.core
      
      val s = new AnalysisActor
      running.set(true)
      stats.set(Some(s))

      val mgImage = inputImage.is.get
      val image = try {
        ImageMatrix.fromFile(mgImage.file)
      } catch {
        case _ =>
          S.error("The uploaded file isn't an image file!")
          S.redirectTo("/image")
      }

      val session = new core.session.AnalysisSession[ImageMatrix](analysers.is.filter(activeAnalysers.is), image)
      val key = ReportManager.makeId
      info("Current report key " + key)
      resultsKey.set(key)
      val actor = Actor.actor {
        try {
          ReportManager.saveReport(session.run(s), key)
        } catch {
          case ex => s.error(ex.getMessage)
        }
        s.done()
      }
      actor.start()
      S.redirectTo("/process")
    }

    def redraw() = SetHtml(id, inner())
    
    def inner(): NodeSeq = { //TODO: only update necessary parts!
      bind("analyserList", analyserListTemplate,
           "entry" -> makeEntry _,
           "submit" -> ((x: NodeSeq) => SHtml.submit(x.text, doSubmit)))
    }
    <div id={id}>{inner()}</div>
  }

  def resultsView(resultsViewTemplate: NodeSeq): NodeSeq = {
    val id = randomString(16)
    val image = inputImage.is.get.load()
    
    val res = (if(results.is.isEmpty) {
        val r = ReportManager.loadReport(resultsKey.is)
        if(r.isEmpty) info("Loaded empty results tree")
        results.set(r)
        r
      } else results.is).map(x => x._1 -> x._2.toSeq.sortWith((x, y) => x.level.name < y.level.name))

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
        val labelResolver = entry match {
          case m: Message => (_: NodeSeq) => Text(m.message)
          case _ => (n: NodeSeq) => n
        }
        
        bind("reportEntry", reportEntryTemplate,
             "message" -> ((n: NodeSeq) => SHtml.a(() => {currentReportEntry = Some(entry); redraw}, labelResolver(n))))
      }.toList

    def makeView(viewTemplate: NodeSeq): NodeSeq = currentReportEntry match {
      case None =>
        inputImage.is.map(x => <img src={S.hostAndPath + "/imagefile/" + x.id}/>) getOrElse viewTemplate
      case Some(entry) if inputImage.is.isDefined =>
        val domImpl = GenericDOMImplementation.getDOMImplementation
        val svgNS = "http://www.w3.org/2000/svg"
        val document = domImpl.createDocument(svgNS, "svg", null)

        val context = SVGGeneratorContext.createDefault(document)
        //context.setImageHandler(SVGImageHandler)
            
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
        //DEBUG:
        XML.save("output.svg", svg, "UTF-8", true, null)
        <div style={"width: " + image.getWidth + "px; height: " + image.getHeight + "px;"}>{svg}</div>
      case _ => viewTemplate
    }
    
    def redraw() = SetHtml(id, inner())

    def inner() = {
      bind("resultsView", resultsViewTemplate,
           "tree" -> makeTree _,
           "view" -> makeView _)
    }
    <div id={id}>{inner()}</div>
  }
}
