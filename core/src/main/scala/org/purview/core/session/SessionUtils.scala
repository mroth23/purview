package org.purview.core.session

import java.util.ServiceLoader
import org.purview.core.analysis.Analyser
import org.purview.core.data.ImageMatrix
import scala.collection.JavaConversions._

object SessionUtils {
  def createAnalyserInstances(): Seq[Analyser[ImageMatrix]] = {
    ServiceLoader.load(classOf[Analyser[_]]).map(_.asInstanceOf[Analyser[ImageMatrix]]).toSeq
  }
}
