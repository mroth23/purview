package org.purview.core.report

import java.awt.image.BufferedImage
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import javax.imageio.ImageIO
import org.apache.commons.codec.binary.Base64
import org.purview.core.analysis.Metadata
import org.purview.core.data._
import org.purview.core.data.shape._
import org.purview.core.data.plot._
import scala.xml._

object ReportPersistance {
  def serializeReport(report: Map[Metadata, Set[ReportEntry]]): Elem =
    (<report>{serializeAnalyserResults(report.toSeq)}</report>)

  def serializeAnalyserResults(analyserResults: Seq[(Metadata, Set[ReportEntry])]) = analyserResults map {analyserResult =>
    val (metadata, entries) = analyserResult
    (<metadata name={metadata.name} description={metadata.description} version={metadata.version.orNull} author={metadata.author.orNull} icon-resource={metadata.iconResource.orNull}>{serializeEntries(entries)}</metadata>)
  }

  def serializeEntries(entries: Set[ReportEntry]): NodeSeq = (entries map {entry =>
    val xml = (entry match {
        case ReportMessage(_, _) =>
          (<entry type="message"/>)
        case ReportImage(_, _, x, y, image) =>
          (<entry type="image" x={x.toString} y={y.toString}>{serializeImage(image)}</entry>)
        case ReportRectangle(_, _, x, y, width, height) =>
          (<entry type="rectangle" x={x.toString} y={y.toString} width={width.toString} height={height.toString}/>)
        case ReportCircle(_, _, x, y, radius) =>
          (<entry type="circle" x={x.toString} y={y.toString} radius={radius.toString}/>)
        case ReportRectangleMove(_, _, srcX, srcY, tgtX, tgtY, w, h) =>
          (<entry type="rectangle-move" width={w.toString} height={h.toString}><source x={srcX.toString} y={srcY.toString}/><target x={tgtX.toString} y={tgtY.toString}/></entry>)
        case ReportCircleMove(_, _, srcX, srcY, tgtX, tgtY, radius) =>
          (<entry type="circle-move" radius={radius.toString}><source x={srcX.toString} y={srcY.toString}/><target x={tgtX.toString} y={tgtY.toString}/></entry>)
        case ReportShape(_, _, entries) =>
          (<entry type="shape">{serializeShapeEntries(entries)}</entry>)
        case ReportShapeMove(_, _, sourceEntries, targetEntries) =>
          (<entry type="shape-move"><source>{serializeShapeEntries(sourceEntries)}</source><target>{serializeShapeEntries(targetEntries)}</target></entry>)
        case ReportPlot(_, _, entries) =>
          (<entry type="plot">{serializePlotEntries(entries)}</entry>)
      })
    xml % new UnprefixedAttribute("message", entry.message, new UnprefixedAttribute("level", serializeReportLevel(entry.level), xml.attributes))
  }).toSeq

  private def serializeReportLevel(level: ReportLevel): String = level match {
    case Debug => "debug"
    case Information => "information"
    case Warning => "warning"
    case Error => "error"
    case Critical => "critical"
  }

  def serializeImage(image: Matrix[Color]): Elem = {
    val storage = new ByteArrayOutputStream
    val out = new BufferedImage(image.width, image.height, BufferedImage.TYPE_INT_ARGB)
    var x = 0
    while(x < image.width) {
      var y = 0
      while(y < image.height) {
        out.setRGB(x, y, image(x, y).toRGB)
        y += 1
      }
      x += 1
    }
    ImageIO.write(out, "png", storage)
    storage.close()
    (<matrix type="color:image/png" width={image.width.toString} height={image.height.toString}>{new String(Base64.encodeBase64(storage.toByteArray), "UTF-8")}</matrix>)
  }

  def serializeShapeEntries(entries: Seq[ShapeCommand]): NodeSeq = entries map {
    case ShapeMoveTo(x, y) =>
      (<command type="move" x0={x.toString} y0={y.toString}/>)
    case ShapeLineTo(x, y) =>
      (<command type="line" x0={x.toString} y0={y.toString}/>)
    case ShapeQuadTo(x0, y0, x1, y1) =>
      (<command type="quad" x0={x0.toString} y0={y0.toString} x1={x1.toString} y1={y1.toString}/>)
    case ShapeCubicTo(x0, y0, x1, y1, x2, y2) =>
      (<command type="cubic" x0={x0.toString} y0={y0.toString} x1={x1.toString} y1={y1.toString} x2={x2.toString} y2={y2.toString}/>)
    case ShapeClose =>
      (<command type="close"/>)
    case ShapeUseOddEvenFill =>
      (<command type="fill-odd-even"/>)
    case ShapeUseWindingFill =>
      (<command type="fill-winding"/>)
  }

  def serializePlotEntries(entries: Seq[PlotEntry]): NodeSeq = entries map {
    case PlotPoint(x, y, z, color) =>
      (<entry type="point" x={x.toString} y={y.toString} z={z.toString} a={color.a.toString} r={color.r.toString} g={color.g.toString} b={color.b.toString}/>)
    case PlotVector(xDir, yDir, zDir, color) =>
      (<entry type="vector" xd={xDir.toString} yd={yDir.toString} zd={zDir.toString} a={color.a.toString} r={color.r.toString} g={color.g.toString} b={color.b.toString}/>)
  }

  def deserializeReport(data: Elem): Map[Metadata, Set[ReportEntry]] =
    deserializeAnalyserResults(data).toMap

  def deserializeAnalyserResults(d: NodeSeq): Set[(Metadata, Set[ReportEntry])] = ((d\"metadata") map {data =>
    val meta = new Metadata {
      val name = (data\"@name").text
      val description = (data\"@description").text
      override val version = (data\"@version").headOption.map(_.text)
      override val author = (data\"@author").headOption.map(_.text)
      override val iconResource = (data\"@icon-resource").headOption.map(_.text)
    }
    val entries = deserializeEntries(data)

    (meta, entries)
  }).toSet

  def deserializeEntries(d: NodeSeq): Set[ReportEntry] = ((d\"entry") map {data =>
    @inline def attr(label: String) = (data\("@"+label)).text
    @inline def fattr(label: String) = attr(label).toFloat
    val message = attr("message")
    val level = deserializeReportLevel(attr("level"))
    (data\"@type").text match {
      case "message" => ReportMessage(level, message)
      case "image" => ReportImage(level, message, fattr("x"), fattr("y"), deserializeImage(data))
      case "rectangle" => ReportRectangle(level, message, fattr("x"), fattr("y"), fattr("width"), fattr("height"))
      case "circle" => ReportRectangle(level, message, fattr("x"), fattr("y"), fattr("width"), fattr("height"))
      case "rectangle-move" =>
        val source = data\"source"
        val target = data\"target"
        ReportRectangleMove(level, message, (source\"@x").text.toFloat, (source\"@y").text.toFloat,
                            (target\"@x").text.toFloat, (target\"@y").text.toFloat,
                            fattr("width"), fattr("height"))
      case "circle-move" =>
        val source = data\"source"
        val target = data\"target"
        ReportCircleMove(level, message, (source\"@x").text.toFloat, (source\"@y").text.toFloat,
                         (target\"@x").text.toFloat, (target\"@y").text.toFloat,
                         fattr("radius"))
      case "shape" =>
        ReportShape(level, message, deserializeShapeEntries(data))
      case "shape-move" =>
        ReportShapeMove(level, message, deserializeShapeEntries(data\"source"), deserializeShapeEntries(data\"target"))
      case "plot" =>
        ReportPlot(level, message, deserializePlotEntries(data))
    }
  }).toSet

  private def deserializeReportLevel(data: String): ReportLevel = data match {
    case "debug" => Debug
    case "information" => Information
    case "warning" => Warning
    case "error" => Error
    case "critical" => Critical
  }

  def deserializeImage(data: NodeSeq): Matrix[Color] = {
    val matrix = data\"matrix"
    require((matrix\"@type").text == "color:image/png", "Wrong matrix type")
    val width = (matrix\"@width").text.toInt
    val height = (matrix\"@height").text.toInt
    val result = new MutableArrayMatrix[Color](width, height)
    val imageData = Base64.decodeBase64(matrix.text.getBytes("UTF-8"))
    val storage = new ByteArrayInputStream(imageData)
    val in = ImageIO.read(storage)
    require(width == in.getWidth, "Size mismatch: width=" + width + " but the data had width=" + in.getWidth)
    require(height == in.getHeight, "Size mismatch: height=" + width + " but the data had height=" + in.getWidth)
    storage.close()
    var y = 0
    while(y < height) {
      var x = 0
      while(x < width) {
        result(x, y) = Color.fromRGB(in.getRGB(x, y))
        x += 1
      }
      y += 1
    }
    result
  }

  def deserializeShapeEntries(data: NodeSeq): Seq[ShapeCommand] = (data\"command") map { command =>
    @inline def fattr(label: String) = (command\("@" + label)).text.toFloat
    (command\"@type").text match {
      case "move" => ShapeMoveTo(fattr("x0"), fattr("y0"))
      case "line" => ShapeLineTo(fattr("x0"), fattr("y0"))
      case "quad" => ShapeQuadTo(fattr("x0"), fattr("y0"), fattr("x1"), fattr("y1"))
      case "cubic" => ShapeCubicTo(fattr("x0"), fattr("y0"), fattr("x1"), fattr("y1"), fattr("x2"), fattr("y2"))
      case "close" => ShapeClose
      case "fill-odd-even" => ShapeUseOddEvenFill
      case "fill-winding" => ShapeUseWindingFill
    }
  }

  def deserializePlotEntries(data: NodeSeq): Seq[PlotEntry] = (data\"entry") map { entry =>
    @inline def fattr(label: String) = (entry\("@" + label)).text.toFloat
    (entry\"@type").text match {
      case "point" => new PlotPoint(fattr("x"), fattr("y"), fattr("z"), Color(fattr("a"), fattr("r"), fattr("g"), fattr("b")))
      case "vector" => new PlotVector(fattr("xd"), fattr("yd"), fattr("zd"), Color(fattr("a"), fattr("r"), fattr("g"), fattr("b")))
    }
  }
}
