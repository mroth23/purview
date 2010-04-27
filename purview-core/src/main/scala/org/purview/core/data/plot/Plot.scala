package org.purview.core.data.plot

import org.purview.core.data.Color

sealed abstract class PlotEntry

sealed case class PlotPoint(x: Float, y: Float, z: Float, color: Color) extends PlotEntry
sealed case class PlotVector(xDir: Float, yDir: Float, zDir: Float, color: Color) extends PlotEntry
