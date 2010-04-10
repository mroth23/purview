package org.purview.webui.util

import java.awt.image.BufferedImage
import java.io.InputStream

object ImageUtils {
  def createInputImage(name: String, stream: InputStream): InputImage = {
    //Save the image:
    val imageHandle = ImageManager.saveImage(stream)
    
    //Load the local image into memory:
    val inputImage = imageHandle.load()

    //Scale the image
    val scaleFactor = (750f / inputImage.getWidth) min (562.5f / inputImage.getHeight) min 1.0f
    val scaledImageHandle = ImageManager.saveImage(scaleImage(inputImage, scaleFactor))

    //Construct our InputImage container
    InputImage(
      name = name,
      original = imageHandle,
      scaled = scaledImageHandle
    )
  }

  def scaleImage(image: BufferedImage, scaleFactor: Float = 1.0f) = {
    val result = new BufferedImage((image.getWidth * scaleFactor).toInt,
                                   (image.getHeight * scaleFactor).toInt, BufferedImage.TYPE_INT_ARGB)
    val g = result.createGraphics
    try {
      g.drawImage(image, 0, 0, (image.getWidth * scaleFactor).toInt, (image.getHeight * scaleFactor).toInt, null)
    } finally {
      g.dispose()
    }
    result
  }
}
