package org.purview.core.analysis

import org.purview.core.data.Color
import org.purview.core.data.Matrix
import org.purview.core.report.Image
import org.purview.core.report.Information
import org.purview.core.report.Message
import org.purview.core.report.Point
import org.purview.core.report.Rectangle
import org.purview.core.report.ReportEntry
import org.purview.core.report.ReportLevel
import org.purview.core.data.MutableMatrix
import org.purview.core.process.Computation
import org.purview.core.transforms.MatrixToImage
import scala.collection.mutable.Queue

/**
 * A HeatMapAnalyser is an Analyser that analyses a Matrix and outputs a heat
 * map as its result. The heat map is a Matrix of Floats, where regions with
 * high values indicate things to report.
 */
trait HeatMapAnalyser[A] extends Analyser[Matrix[A]] {
  /** The computation that generates the heat map */
  def heatmap: Computation[Matrix[Float]]

  /** The report level to use by default when creating a report */
  def reportLevel: ReportLevel = Information

  /** The message to use for interesting heat map areas in the report */
  def message: String = "Interesting peak"

  /**
   * If multiple heat regions are to be found, this specifies the maximum
   * distance ratio that other regions may be from the strongest region.
   * So, if the strongest region has value <pre>x</pre>, all regions
   * <pre>r</pre> will be included in the report that satisfy:<br/>
   * <pre>abs(max - r) < max * maxDeviationTolerance</pre>
   */
  def maxDeviationTolerance: Float = 0.1f

  /**
   * Specifies the minimum size a heat map region must have for it to be accepted.
   */
  def minRegionSize: Int = 8

  /**
   * Specifies the minimum value a peak in the heat map must have for it to be accepted.
   */
  def threshold: Float = 0

  /**
   * Should nearby peaks be treated as the same peak region?
   */
  def accumulate = true

  private def maximi = 
    for {
      in <- heatmap
      _ = status("Finding peaks in the generated data")
      max = in.max
      tolerance = max * maxDeviationTolerance
    } yield (in, in map(value => value - max < tolerance && max - value < tolerance && value > threshold))

  protected case class HeatRegion(var left: Int, var top: Int, var right: Int, var bottom: Int)

  protected def heatRegions = for(max <- maximi; (in, candidateMatrix) = max) yield {
    status("Calculating " + (if(accumulate) "and merging " else "") + "peak regions")
    val mask = new MutableMatrix[Boolean](candidateMatrix.width, candidateMatrix.height)
    val width = candidateMatrix.width
    val height = candidateMatrix.height

    @inline def free(x: Int, y: Int) = candidateMatrix(x, y) && !mask(x, y)

    @inline def include(x: Int, y: Int, heatRegion: HeatRegion) = {
      mask(x, y) = true
      if(x < heatRegion.left)
        heatRegion.left = x
      if(x > heatRegion.right)
        heatRegion.right = x
      if(y < heatRegion.top)
        heatRegion.top = y
      if(y > heatRegion.bottom)
        heatRegion.bottom = y
    }

    /** A standard flood-fill algorithm */
    @inline def floodFrom(x: Int, y: Int, heatRegion: HeatRegion): Unit = {
      val queue = new Queue[(Int, Int)]
      queue.enqueue((x, y))

      while(!queue.isEmpty) {
        val pos = queue.dequeue
        include(pos._1, pos._2, heatRegion)

        var minus, plus = pos._1
        while(minus > 1 && free(minus - 1, pos._2)) minus -= 1
        while(plus < width - 1 && free(plus + 1, pos._2)) plus += 1

        while(minus <= plus) {
          include(minus, pos._2, heatRegion)

          if(pos._2 > 1 && free(minus, pos._2 - 1))
            queue.enqueue((minus, pos._2 - 1))
          if(pos._2 < height - 1 && free(minus, pos._2 + 1))
            queue.enqueue((minus, pos._2 + 1))
          
          minus += 1
        }
      }
    }

    var result: List[HeatRegion] = Nil
    var y = 0
    while( y < candidateMatrix.height) {
      var x = 0
      while(x < candidateMatrix.width) {
        if(free(x, y)) {
          val r = new HeatRegion(x, y, x, y)
          if(accumulate)
            floodFrom(x, y, r)
          result ::= r
        }
        x += 1
      }
      y += 1
    }

    (in, result)
  }

  def result = {
    for(r <- heatRegions; (in, regions) = r) yield regions
    .filter(r => r.bottom - r.top >= minRegionSize && r.right - r.left >= minRegionSize)
    .map { region =>
      new ReportEntry with Point with Rectangle with Message {
        val message = HeatMapAnalyser.this.message
        val x = (region.left + region.right) / 2
        val y = (region.top + region.bottom) / 2
        val width = (region.right - region.left)
        val height = (region.bottom - region.top)
        val level = reportLevel
      }
    }.toSet + {
      val max = in.max
      new ReportEntry with Point with Image with Message {
        val message = "Raw output"
        val level = Information
        val x = 0
        val y = 0
        val image = new MatrixToImage()(in.map(x => Color(1, x / max, x / max, x / max)))
      }
    }
  }
}

trait HeatMapImageAnalyser extends HeatMapAnalyser[Color]
