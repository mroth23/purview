package org.purview.webui.util

import java.awt.image.BufferedImage
import java.awt.image.RenderedImage
import java.io.BufferedInputStream
import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.InputStream
import java.nio.channels.Channel
import java.nio.channels.Channels
import java.nio.channels.ReadableByteChannel
import javax.imageio.ImageIO
import net.liftweb.common.Box
import net.liftweb.common.Full
import net.liftweb.http.GoneResponse
import net.liftweb.http.LiftResponse
import net.liftweb.http.StreamingResponse
import net.liftweb.util.Helpers

object ImageManager extends FileManager {
  def loadImage(id: String): Option[Image] = {
    val file = createFile(id)
    if(file.exists)
      Some(new FileImage(id, file))
    else
      None
  }

  def serveImage(id: String): Box[LiftResponse] = {
    val file = createFile(id)
    if(file.exists) {
      val input = new FileInputStream(file)
      Full(StreamingResponse(input, input.close, file.length, ("Content-Type" -> "image/png") :: Nil, Nil, 200))
    }
    else
      Full(GoneResponse())
  }

  def saveImage(stream: InputStream): Image = {
    val id = Helpers.nextFuncName
    val file = createFile(id)
    file.createNewFile()

    val in = Channels.newChannel(stream)
    val out = new FileOutputStream(file).getChannel
    try {
      out.transferFrom(in, 0, Long.MaxValue)
    } finally {
      out.close()
    }

    new FileImage(id, file)
  }

  def saveImage(image: RenderedImage): Image = {
    val id = Helpers.nextFuncName
    val file = createFile(id)
    ImageIO.write(image, "png", file)
    new FileImage(id, file)
  }

  def saveImage(in: ReadableByteChannel): Image = {
    val id = Helpers.nextFuncName
    val file = createFile(id)
    val out = new FileOutputStream(file).getChannel
    try {
      out.transferFrom(in, 0, Long.MaxValue)
    } finally {
      out.close()
    }
    new FileImage(id, file)
  }

  private class FileImage(val id: String, val file: File) extends Image {
    def load() = ImageIO.read(file)
    def createBinaryStream() = new BufferedInputStream(new FileInputStream(file))
    def createChannel() = new FileInputStream(file).getChannel
  }

  trait Image {
    val id: String
    def load(): BufferedImage
    def createBinaryStream(): InputStream
    def createImageStream() = ImageIO.createImageInputStream(createBinaryStream())
    def createChannel(): Channel
    def file: File
  }
}
