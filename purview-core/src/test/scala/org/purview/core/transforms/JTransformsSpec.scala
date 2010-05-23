package org.purview.core.transforms

import org.purview.core.data.ImmutableMatrix
import org.specs.SpecificationWithJUnit

class JTransformsSpec extends SpecificationWithJUnit {
  "A 1D discrete cosine transform" should {
    "be reversible" in {
      val data = Array(-76f,-73f,-67f,-62f,-58f,-67f,-64f,-55f)
      val dct = JTransformsForwardDCT1D()
      val idct = JTransformsInverseDCT1D()
      val reconstructed = idct(dct(data))
      data zip reconstructed forall (x => x._1 must_== x._2)
    }
  }

//    "A 2D discrete cosine transform" should {
//    "be reversible" in {
//      val data = ImmutableMatrix(8,8, Array(-76f,-73f,-67f,-62f,-58f,-67f,-64f,-55f,
//                                            -65f,-69f,-73f,-38f,-19f,-43f,-59f,-56f,
//                                            -66f,-69f,-60f,-15f, 16f,-24f,-62f,-55f,
//                                            -65f,-70f,-57f, -6f, 26f,-22f,-58f,-59f,
//                                            -61f,-67f,-60f,-24f, -2f,-40f,-60f,-58f,
//                                            -49f,-63f,-68f,-58f,-51f,-60f,-70f,-53f,
//                                            -43f,-57f,-64f,-69f,-73f,-67f,-63f,-45f,
//                                            -41f,-49f,-59f,-60f,-63f,-52f,-50f,-34f))
//      val dct = JTransformsForwardDCT2D()
//      val idct = JTransformsInverseDCT2D()
//      val reconstructed = idct(dct(data))
//      data zip reconstructed forall (x => x._1 must_== x._2)
//    }
//  }

}
