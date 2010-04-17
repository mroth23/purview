package org.purview.webui.util

import java.awt.image.BufferedImage
import java.io.FileOutputStream
import java.io.InputStream
import java.nio.channels.Channels
import javax.imageio.ImageIO

object ImageUtils {
  def makeImageSet(stream: InputStream, handle: String) = {
    val rawFile = UploadManager.file(handle)
    rawFile.createNewFile()

    {
      val in = Channels.newChannel(stream)
      val out = new FileOutputStream(rawFile).getChannel
      try out.transferFrom(in, 0, Long.MaxValue) finally out.close()
    }

    val optimizedImage = ImageIO.read(rawFile)
    val optimizedId = handle + "-optimized"
    ImageManager.write(optimizedId, optimizedImage)

    val scaleFactor = (750f / optimizedImage.getWidth) min (562.5f / optimizedImage.getHeight) min 1.0f
    val scaledImage = scaleImage(optimizedImage, scaleFactor)
    val scaledId = handle + "-scaled"
    ImageManager.write(scaledId, scaledImage)
  }

  def scaleImage(image: BufferedImage, scaleFactor: Float = 1.0f) = {
    val result = new BufferedImage((image.getWidth * scaleFactor).toInt,
                                   (image.getHeight * scaleFactor).toInt, BufferedImage.TYPE_INT_ARGB)
    val g = result.createGraphics
    try
    g.drawImage(image, 0, 0, (image.getWidth * scaleFactor).toInt, (image.getHeight * scaleFactor).toInt, null)
    finally
    g.dispose()
    result
  }
}
