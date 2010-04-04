package org.purview.core.analysis

import org.purview.core.data.Color
import org.purview.core.data.ImageMatrix
import org.purview.core.data.Matrix
import org.purview.core.report.Image
import org.purview.core.report.Information
import org.purview.core.report.Message
import org.purview.core.report.Point
import org.purview.core.report.Rectangle
import org.purview.core.report.ReportEntry
import org.purview.core.report.ReportLevel
import org.purview.core.data.MutableArrayMatrix
import org.purview.core.process.Computation
import org.purview.core.transforms.MatrixToImage
import org.purview.core.transforms.LinearConvolve
import scala.collection.mutable.Queue

/**
 * A HeatMapAnalyser is an Analyser that analyses a Matrix and outputs a heat
 * map as its result. The heat map is a Matrix of Floats, where regions with
 * high values indicate things to report.
 */
trait HeatMapAnalyser[@specialized("Int,Float,Boolean") A, B <: Matrix[A]]
    extends Analyser[B] {

  /** The computation that generates the heat map */
  val heatmap: Computation[Matrix[Float]]

  /** The report level to use by default when creating a report */
  val reportLevel: ReportLevel = Information

  /** The message to use for interesting heat map areas in the report */
  val message: String = "Interesting peak"

  /** Should we convolve the result before scanning it? */
  val convolve: Computation[Option[Array[Float]]] = Computation(None)

  /**
   * If multiple heat regions are to be found, this specifies the maximum
   * distance ratio that other regions may be from the strongest region.
   * So, if the strongest region has value <pre>x</pre>, all regions
   * <pre>r</pre> will be included in the report that satisfy:<br/>
   * <pre>abs(max - r) &lt; max * maxDeviationTolerance</pre>
   */
  val maxDeviationTolerance: Float = 0.1f

  /**
   * Specifies the minimum size a heat map region must have for it 
   * to be accepted.
   */
  val minRegionSize: Int = 8

  /**
   * Specifies the minimum value a peak in the heat map must have
   * for it to be accepted.
   */
  val threshold: Float = 0

  /**
   * Should nearby peaks be treated as the same peak region?
   */
  val accumulate = true

  /** Heatmap with the optional convolution row matrix applied */
  private lazy val convolvedHeatmap = for {
    raw <- heatmap
    conv <- convolve
  } yield if(conv.isDefined) LinearConvolve(conv.get)(raw) else raw

  /**
   * A matrix that has 'true' cells for "heated" areas and 'false'
   * for other areas
   */
  private lazy val maximi =
    for {
      in <- convolvedHeatmap
      _ = status("Finding peaks in the generated data")
      max = in.max
      tolerance = max * maxDeviationTolerance
    } yield in map(value => value - max < tolerance &&
                            max - value < tolerance &&
                            value > threshold)

  /** Simple helper class */
  protected case class HeatRegion(var left: Int, var top: Int, 
                                  var right: Int, var bottom: Int)

  /** A sequence of actual found heat areas */
  protected lazy val heatRegions = for(candidateMatrix <- maximi) yield {
    status("Calculating " + (if(accumulate) "and merging " else "") +
           "peak regions")
    val mask = new MutableArrayMatrix[Boolean](candidateMatrix.width,
                                               candidateMatrix.height)
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
        val pos = queue.dequeue()
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

    result
  }

  /** A report of all the found heat areas */
  private lazy val regionReport: Computation[Set[ReportEntry]] = for {
    regions <- heatRegions
  } yield (for {
    region <- regions
    if(region.bottom - region.top  >= minRegionSize &&
       region.right  - region.left >= minRegionSize)
  } yield {
    new ReportEntry with Point with Rectangle with Message {
      val message = HeatMapAnalyser.this.message
      val x = region.left
      val y = region.top
      val width = (region.right - region.left)
      val height = (region.bottom - region.top)
      val level = reportLevel
    }
  }).toSet

  /** The generated result report */
  lazy val result: Computation[Set[ReportEntry]] = for {
    raw <- heatmap
    conv <- convolvedHeatmap
    report <- regionReport
  } yield report + { //The unconvoluted input image
    val max = raw.max //TODO: this has already been calculated before, OPTIMIZE!
    (new ReportEntry with Point with Image with Message {
        val message = "Raw output"
        val level = Information
        val x = 0
        val y = 0
        val image = new MatrixToImage()(raw.map { x => 
          Color(0.9f, x / max, x / max, x / max)
        })
      }): ReportEntry
  } + { //The convoluted input image
    val max = conv.max
    (new ReportEntry with Point with Image with Message {
        val message = "Convoluted output"
        val level = Information
        val x = 0
        val y = 0
        val image = new MatrixToImage()(conv.map {x =>
          Color(0.9f, x / max, x / max, x / max)
        })
      }): ReportEntry
  }
}

trait HeatMapImageAnalyser extends HeatMapAnalyser[Color, ImageMatrix]
