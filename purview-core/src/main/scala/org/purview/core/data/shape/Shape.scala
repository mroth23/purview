package org.purview.core.data.shape

sealed abstract class ShapeCommand

sealed case class ShapeMoveTo(x: Float, y: Float) extends ShapeCommand
sealed case class ShapeLineTo(x: Float, y: Float) extends ShapeCommand
sealed case class ShapeQuadTo(x0: Float, y0: Float, x1: Float, y1: Float) extends ShapeCommand
sealed case class ShapeCubicTo(x0: Float, y0: Float, x1: Float, y1: Float, x2: Float, y2: Float) extends ShapeCommand
case object ShapeClose extends ShapeCommand

case object ShapeUseOddEvenFill extends ShapeCommand
case object ShapeUseWindingFill extends ShapeCommand