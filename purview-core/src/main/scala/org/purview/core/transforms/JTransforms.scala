package org.purview.core.transforms

import edu.emory.mathcs.jtransforms._
import org.purview.core.data.Complex
import org.purview.core.data.ImmutableMatrix
import org.purview.core.data.Matrix

case class JTransformsForwardDCT1D() extends Function1[Seq[Float], Seq[Float]] {

  def apply(data: Seq[Float]): Seq[Float] = {
    var array = data.toArray
    val dct = new edu.emory.mathcs.jtransforms.dct.FloatDCT_1D(data.length)
    dct.forward(array, 0, false)
    array.toSeq
  }
}

case class JTransformsInverseDCT1D() extends Function1[Seq[Float], Seq[Float]] {

  def apply(data: Seq[Float]): Seq[Float] = {
    var array = data.toArray
    val dct = new edu.emory.mathcs.jtransforms.dct.FloatDCT_1D(data.length)
    dct.inverse(array, 0, false)
    array.toSeq
  }

}

case class JTransformsForwardDCT2D() extends Function1[Matrix[Float], Matrix[Float]] {

  def apply(data: Matrix[Float]): Matrix[Float] = {
    var array = data.toArray
    val dct = new edu.emory.mathcs.jtransforms.dct.FloatDCT_2D(data.width, data.height)
    dct.forward(array, false)
    new ImmutableMatrix[Float](data.width, data.height, array)
  }

}

case class JTransformsInverseDCT2D() extends Function1[Matrix[Float], Matrix[Float]] {

  def apply(data: Matrix[Float]): Matrix[Float] = {
    var array = data.toArray
    val dct = new edu.emory.mathcs.jtransforms.dct.FloatDCT_2D(data.width, data.height)
    dct.inverse(array, false)
    new ImmutableMatrix[Float](data.width, data.height, array)
  }

}

//case class JTransformsForwardFFT1D() extends Function1[Seq[Float], Seq[Float]] {
//
//  def apply(data: Seq[Float]): Seq[Complex[Float]] = {
//    var array = data.toArray
//    val fft = new edu.emory.mathcs.jtransforms.fft.FloatFFT_1D(data.length)
//    fft.realForwardFull(array, 0)
////    (for(x : Int <- 0 until array.length) yield {
////
////    })
//  }
//}

case class JTransformsInverseFFT1D() extends Function1[Seq[Float], Seq[Float]] {

  def apply(data: Seq[Float]): Seq[Float] = {
    var array = data.toArray
    val dct = new edu.emory.mathcs.jtransforms.dct.FloatDCT_1D(data.length)
    dct.inverse(array, 0, false)
    array.toSeq
  }

}

