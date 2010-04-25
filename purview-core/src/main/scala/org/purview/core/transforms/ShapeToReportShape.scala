package org.purview.core.transforms

import java.awt.Shape
import java.awt.geom.PathIterator
import org.purview.core.data.shape._
import scala.collection.mutable.ListBuffer

case class ShapeToReportShape() extends Function1[Shape, Seq[ShapeCommand]] {
  def apply(input: Shape) = {
    val result = new ListBuffer[ShapeCommand]
    val iter = input.getPathIterator(null)
    val buffer = new Array[Float](6)
    var prevWind = -1
    
    while(!iter.isDone) {
      iter.getWindingRule match {
        case PathIterator.WIND_EVEN_ODD if prevWind != PathIterator.WIND_EVEN_ODD =>
          result += ShapeUseOddEvenFill
          prevWind = PathIterator.WIND_EVEN_ODD
        case PathIterator.WIND_NON_ZERO if prevWind != PathIterator.WIND_NON_ZERO =>
          result += ShapeUseWindingFill
          prevWind = PathIterator.WIND_NON_ZERO
        case _ => //nothing (needed because we use prevWind)
      }
      
      iter.currentSegment(buffer) match {
        case PathIterator.SEG_MOVETO =>
          result += ShapeMoveTo(buffer(0), buffer(1))
        case PathIterator.SEG_LINETO =>
          result += ShapeLineTo(buffer(0), buffer(1))
        case PathIterator.SEG_QUADTO =>
          result += ShapeQuadTo(buffer(0), buffer(1), buffer(2), buffer(3))
        case PathIterator.SEG_CUBICTO =>
          result += ShapeCubicTo(buffer(0), buffer(1), buffer(2), buffer(3), buffer(4), buffer(5))
        case PathIterator.SEG_CLOSE =>
          result += ShapeClose
      }
      iter.next()
    }
    result.toList
  }
}
