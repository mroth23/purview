package org.purview.core.session

import java.util.ServiceLoader
import org.purview.core.analysis.Analyser
import scala.collection.JavaConversions._

object SessionUtils {
  /**
   * Loads all available analysers as instances of Analyser[A]
   *
   * WARNING: all analysers will be loaded, regardless of type parameter.
   * Please ensure that the classpath doesn't contain modules that extend Analyser[B]
   */
  def createAnalyserInstances[A](): Seq[Analyser[A]] = {
    ServiceLoader.load(classOf[Analyser[_]]).map(_.asInstanceOf[Analyser[A]]).toSeq
  }
}
