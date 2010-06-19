package org.purview.core.data.plot

import org.purview.core.data.Color

sealed abstract class PlotEntry

sealed case class PlotPoint3D(x: Float, y: Float, z: Float, color: Color) extends PlotEntry
sealed case class PlotVector3D(xDir: Float, yDir: Float, zDir: Float, color: Color) extends PlotEntry
sealed case class PlotPoint2D(x: Float, y: Float, color: Color) extends PlotEntry
sealed case class PlotVector2D(x: Float, y: Float, color: Color) extends PlotEntry
