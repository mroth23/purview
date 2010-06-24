package org.purview.core.transforms

import org.specs.SpecificationWithJUnit
class JTransformsSpec extends SpecificationWithJUnit {

  "A 1D discrete cosine transform" should {
    "be reversible" in {
	val rnd = new scala.util.Random
      val data = Array.fill(16){ rnd.nextInt(256).toFloat }
      val dct = new JTransforms1D(16)
      val reconstructed = dct.DCT1DInverse(dct.DCT1DForward(data, true), true)
      data zip reconstructed forall (x => x._1.round must_== x._2.round)
    }
  }

  "A 2D discrete cosine transform" should {
    "be reversible" in {
	val rnd = new scala.util.Random
      val data = Array.fill(16, 16){ rnd.nextInt(256).toFloat }
      val dct = new JTransforms2D(16, 16)
      val reconstructed = dct.DCT2DInverse(dct.DCT2DForward(data, true), true)
      data zip reconstructed forall (x => x._1 zip x._2 forall (y => y._1.round must_== y._2.round))
    }
  }

  "A 3D discrete cosine transform" should {
    "be reversible" in {
	val rnd = new scala.util.Random
      val data = Array.fill(16, 16, 16){ rnd.nextInt(256).toFloat }
      val dct = new JTransforms3D(16, 16, 16)
      val reconstructed = dct.DCT3DInverse(dct.DCT3DForward(data, true), true)
      data zip reconstructed forall (x => x._1 zip x._2 forall (y => y._1 zip y._2 forall(z => z._1.round must_== z._2.round)))
    }
  }

  "A 1D fast fourier transform" should {
    "be reversible" in {
	val rnd = new scala.util.Random
      val data = Array.fill(32){ rnd.nextInt(256).toFloat }
      val fft = new JTransforms1D(16)
      val reconstructed = fft.FFT1DInverse(fft.FFT1DForward(data), true)
      data zip reconstructed forall (x => x._1.round must_== x._2.round)
    }
  }

  "A 2D fast fourier transform" should {
    "be reversible" in {
	val rnd = new scala.util.Random
      val data = Array.fill(16, 32){ rnd.nextInt(256).toFloat }
      val fft = new JTransforms2D(16, 16)
      val reconstructed = fft.FFT2DInverse(fft.FFT2DForward(data), true)
      data zip reconstructed forall (x => x._1 zip x._2 forall (y => y._1.round must_== y._2.round))
    }
  }

   "A 3D fast fourier transform" should {
    "be reversible" in {
	val rnd = new scala.util.Random
      val data = Array.fill(16, 16, 32){ rnd.nextInt(256).toFloat }
      val fft = new JTransforms3D(16, 16, 16)
      val reconstructed = fft.FFT3DInverse(fft.FFT3DForward(data), true)
      data zip reconstructed forall (x => x._1 zip x._2 forall (y => y._1 zip y._2 forall(z => z._1.round must_== z._2.round)))
    }
  }

}
