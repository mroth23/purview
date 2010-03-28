package org.purview.webui.util

import java.io.File

class FileManager {
  private val tempDir = new File(System.getProperty("java.io.tmpdir"))
  private val prefix = "purview-"

  protected def createFile(id: String) = new File(tempDir, prefix + id)
}
