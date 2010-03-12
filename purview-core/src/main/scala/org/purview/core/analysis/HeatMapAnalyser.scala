package org.purview.core.analysis

import org.purview.core.data.Color
import org.purview.core.data.Matrix
import org.purview.core.report.Information
import org.purview.core.report.Message
import org.purview.core.report.Point
import org.purview.core.report.ReportEntry
import org.purview.core.report.ReportLevel
import org.purview.core.process.Computation
import scala.util.Sorting

//TODO: this needs fixing!!

/**
 * A HeatMapAnalyser is an Analyser that analyses a Matrix and outputs a heat
 * map as its result. The heat map is a Matrix of Floats, where regions with
 * high values indicate things to report.
 */
trait HeatMapAnalyser[A] extends Analyser[Matrix[A]] {
  /** The computation that generates the heat map */
  val heatmap: Computation[Matrix[Float]]

  /** The report level to use by default when creating a report */
  val defaultReportLevel: ReportLevel = Information

  /** The message to use for interesting heat map areas in the report */
  val message: String = "Interesting peak"

  /**
   * If multiple heat regions are to be found, this specifies the maximum
   * distance ratio that other regions may be from the strongest region.
   * So, if the strongest region has value <pre>x</pre>, all regions
   * <pre>r</pre> will be included in the report that satisfy:<br/>
   * <pre>abs(max - r) < max * maxDeviationTolerance</pre>
   */
  val maxDeviationTolerance: Float = 0.1f

  /**
   * Specifies whether maximi that are next to each other should be merged
   */
  val accumulate: Boolean = true

  private def maximi(in: Matrix[Float], maxDevTol: Float): Set[(Int, Int)] = {
    //TODO: more rigorous way of finding all maximi
    val max = in.max
    val tolerance = max * maxDevTol
    var maximi = Set[(Int, Int)]()

    var y = 0
    while(y < in.height) {
      var x = 0
      while(x < in.width) {
        val cell = in(x, y)
        if(cell - max < tolerance && max - cell < tolerance)
          maximi += ((x, y))
        x += 1
      }
      y += 1
    }
    maximi
  }

  protected def heatRegions(in: Matrix[Float]): Set[(Int, Int)] = {
    val candidates = maximi(in, maxDeviationTolerance)

    //TODO: accumulation algorithm!

    candidates
  }

  def result = for(r <- heatmap) yield {
    val tops = heatRegions(r)

    val entries: Set[ReportEntry] = tops.map { point =>
      new ReportEntry with Point with Message {
        val message = HeatMapAnalyser.this.message
        val x = point._1
        val y = point._2
        val level = reportLevelForValue(r(x, y))
      }
    }

    entries
  }

  /**
   * Provides a report level depending on the amplitude of the input
   */
  def reportLevelForValue(value: Float) = defaultReportLevel
}

trait HeatMapImageAnalyser extends HeatMapAnalyser[Color]

/**
 * Adds a threshold to the normal HeatMapAnalyser. Only regions with values above
 * the treshold will be reported, and additional thresholds control the report
 * levels.
 */
trait FilteredHeatMapAnalyser[A] extends HeatMapAnalyser[A] {
  /** The threshold to use */
  val threshold: Float

  /** Specifies which report level to use for values above a certain threshold */
  val reportLevelThresholds: Map[Float, ReportLevel] = Map()

  abstract override def heatRegions(in: Matrix[Float]): Set[(Int, Int)] =
    super.heatRegions(in).filter(x => in.apply(x._1, x._2) > threshold)

  private val sortedLevels = {
    val result = reportLevelThresholds.keySet.toArray
    Sorting.quickSort(result)
    result
  }
  
  override def reportLevelForValue(value: Float) =
    sortedLevels find {value > _} map reportLevelThresholds getOrElse defaultReportLevel
}

trait FilteredHeatMapImageAnalyser extends FilteredHeatMapAnalyser[Color]
