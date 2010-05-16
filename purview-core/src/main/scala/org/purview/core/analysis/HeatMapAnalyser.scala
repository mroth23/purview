package org.purview.core.analysis

import java.awt.geom.Area
import java.awt.geom.Rectangle2D
import org.purview.core.data.Color
import org.purview.core.data.ImageMatrix
import org.purview.core.data.Matrix
import org.purview.core.report.Information
import org.purview.core.report.ReportEntry
import org.purview.core.report.ReportImage
import org.purview.core.report.ReportLevel
import org.purview.core.data.Computation
import org.purview.core.data.MutableArrayMatrix
import org.purview.core.report.ReportShape
import org.purview.core.transforms.LinearConvolve
import org.purview.core.transforms.ShapeToReportShape
import scala.collection.mutable.Queue
import scala.math._

/**
 * A HeatMapAnalyser is an Analyser that analyses a Matrix and outputs a heat
 * map as its result. The heat map is a Matrix of Floats, where regions with high
 * values indicate things to report.
 */
trait HeatMapAnalyser[@specialized(Int, Float, Boolean) A, B <: Matrix[A]] extends Analyser[B] {

  /** The computation that generates the heat map */
  val heatmap: Computation[Matrix[Float]]

  /** The report level to use by default when creating a report */
  val reportLevel: ReportLevel = Information

  /** The message to use for interesting heat map areas in the report */
  val message: String = "Interesting peak"

  val gaussianStrength = 30
  private val sigma = gaussianStrength / 3
  private val gaussianKernel = (for(i <- -gaussianStrength to gaussianStrength) yield ((1 / sqrt(2 * Pi * sigma * sigma)) * exp(-((i * i) / (2 * sigma * sigma)))).toFloat).toArray

  val convolve: Computation[Option[Array[Float]]] = Computation(Some(gaussianKernel))

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

  /** Should nearby peaks be treated as the same peak region? */
  val accumulate = true

  /** Heatmap with the optional convolution row matrix applied */
  private lazy val convolvedHeatmap = for {
    raw <- heatmap
    conv <- convolve
    _ = status("Convolving the result matrix")
  } yield if(conv.isDefined) LinearConvolve(conv.get)(raw) else raw //Only convolve if conv == Some(...)

  private lazy val convolvedMaxValue = for(in <- convolvedHeatmap) yield Matrix.sequence(in).max

  /**
   * A matrix that has 'true' cells for "heated" areas and 'false'
   * for other areas
   */
  private lazy val maximi =
    for {
      in <- convolvedHeatmap
      max <- convolvedMaxValue
      _ = status("Finding peaks in the generated data")
      tolerance = max * maxDeviationTolerance
    } yield in map(value => value - max < tolerance && //true if we accept the value, false otherwise
                   max - value < tolerance &&
                   value > threshold)

  /** A sequence of actual found heat areas */
  protected lazy val heatAreas = for(candidateMatrix <- maximi) yield {
    status("Calculating " + (if(accumulate) "and merging " else "") +
           "peak regions")

    //A mask that marks already filled cells
    val mask = new MutableArrayMatrix[Boolean](candidateMatrix.width,
                                               candidateMatrix.height)
    val width = candidateMatrix.width
    val height = candidateMatrix.height

    /** Checks whether the specified cell is available to be filled */
    @inline def free(x: Int, y: Int) = candidateMatrix(x, y) && !mask(x, y)

    /** Includes the specified cell in the active heat region */
    @inline def include(x: Int, y: Int, heatArea: Area) = {
      mask(x, y) = true
      heatArea.add(new Area(new Rectangle2D.Float(x - 0.05f, y - 0.05f, 1.1f, 1.1f)))
    }

    /** A standard flood-fill algorithm */
    @inline def floodFrom(x: Int, y: Int, heatArea: Area): Unit = {
      //Cells to be filled
      val queueX = new Queue[Int]
      val queueY = new Queue[Int]

      //"Drop" the flooder at the current point
      queueX.enqueue(x)
      queueY.enqueue(y)

      while(!queue.isEmpty) {
        //Take the next position to fill
        val x = queueX.dequeue()
        val y = queueY.dequeue()

        if(free(x, y)) {
          //Fill the cell
          include(x, y, heatArea)

          //Go to the left and right and find all empty cells
          var minus, plus = pos._1
          while(minus > 1 && free(minus - 1, y)) minus -= 1
          while(plus < width - 1 && free(plus + 1, y)) plus += 1

          //Fill all cells in the same row
          while(minus <= plus) {
            include(minus, y, heatArea)

            //Is the cell above or below available too? Check it later
            if(pos._2 > 1 && free(minus, y - 1))
              queue.enqueue((minus, y - 1))
            if(pos._2 < height - 1 && free(minus, y + 1))
              queue.enqueue((minus, y + 1))

            minus += 1
          }
        }
      }
    }

    //Find all interesting regions by flood-filling them
    var result: List[Area] = Nil
    var y = 0
    while( y < candidateMatrix.height) {
      var x = 0
      while(x < candidateMatrix.width) {
        if(free(x, y)) {
          val a = new Area
          if(accumulate)
            floodFrom(x, y, a)
          result ::= a
        }
        x += 1
      }
      y += 1
    }

    result
  }

  /** A report of all the found heat areas */
  private lazy val regionReport: Computation[Set[ReportEntry]] = for {
    regions <- heatAreas
  } yield {
    val entries = for {
      region <- regions
      if region.getBounds.width  >= minRegionSize
      if region.getBounds.height >= minRegionSize
    } yield
      new ReportShape(reportLevel, HeatMapAnalyser.this.message, ShapeToReportShape()(region))
    entries.toSet
  }

  def getColor(f: Float) : Color = {
    val pi = Pi.toFloat
    val b = cos(f * pi).toFloat
    val g = cos((f - 1f / 2f) * pi).toFloat
    val r = cos((f - 1f) * pi).toFloat
    Color(0.9f, r,  g, b)
  }

  /** The generated result report */
  lazy val result: Computation[Set[ReportEntry]] = for {
    raw <- heatmap
    conv <- convolvedHeatmap
    report <- regionReport
  } yield report + { //The unconvoluted input image
    val max = Matrix.sequence(raw).max //TODO: this has already been calculated before, OPTIMIZE!
    new ReportImage(Information, "Raw output", 0, 0,
                    raw.map { x => getColor(x / max)})
  } + { //The convoluted input image
    val max = Matrix.sequence(conv).max
    new ReportImage(Information, "Convoluted output", 0, 0,
                    conv.map { x => getColor(x / max)})
  }
}

trait HeatMapImageAnalyser extends HeatMapAnalyser[Color, ImageMatrix]
