package org.purview.webui.util

import java.awt.Image
import java.awt.image.BufferedImage
import java.awt.image.RenderedImage
import java.awt.image.renderable.RenderableImage
import net.liftweb.http.S
import org.apache.batik.svggen.SVGSyntax._
import org.apache.batik.util.SVGConstants._
import org.apache.batik.util.XMLConstants._
import org.apache.batik.svggen.DefaultImageHandler
import org.apache.batik.svggen.SVGGeneratorContext
import org.w3c.dom.Element
import scala.collection.mutable.WeakHashMap

object SVGImageHandler extends DefaultImageHandler {
  private val imageHandles = new WeakHashMap[Int, String]

  override def handleHREF(image: Image, imageElement: Element, generatorContext: SVGGeneratorContext) = {
    require(image != null)
    val width = image.getWidth(null)
    val height = image.getHeight(null)

    if(width == 0 || height == 0)
      handleEmptyImage(imageElement)
    else {
      image match {
        case renderedImage: RenderedImage =>
          handleHREF(renderedImage: RenderedImage /* Force overload */, imageElement, generatorContext)
        case _ =>
          val img = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)

          val g = img.createGraphics
          g.drawImage(image, 0, 0, null)
          g.dispose()
          handleHREF(img: RenderedImage /* Force overload */, imageElement, generatorContext)
      }
    }
  }

  override def handleHREF(image: RenderableImage, imageElement: Element, generatorContext: SVGGeneratorContext) = {
    require(image != null)

    val r = image.createDefaultRendering

    if(r == null)
      handleEmptyImage(imageElement)
    else
      handleHREF(r, imageElement, generatorContext)
  }

  override def handleHREF(image: RenderedImage, imageElement: Element, generatorContext: SVGGeneratorContext) = {
    val hash = image.hashCode
    val handle = imageHandles.get(hash) getOrElse {
      val id = ImageManager.makeId
      ImageManager.write(id, image)
      imageHandles(hash) = id
      id
    }

    imageElement.setAttributeNS(XLINK_NAMESPACE_URI, ATTR_XLINK_HREF, S.hostAndPath + "/image/" + handle + ".png")
  }

  private def handleEmptyImage(imageElement: Element) = {
    imageElement.setAttributeNS(XLINK_NAMESPACE_URI, ATTR_XLINK_HREF, DATA_PROTOCOL_PNG_PREFIX)
    imageElement.setAttributeNS(null, SVG_WIDTH_ATTRIBUTE, "0")
    imageElement.setAttributeNS(null, SVG_HEIGHT_ATTRIBUTE, "0")
  }
}
