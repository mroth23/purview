package org.purview.core.transforms

import org.purview.core.data.ImmutableMatrix
import org.specs.SpecificationWithJUnit

class DCTSpec extends SpecificationWithJUnit {
  "A discrete cosine transform" should {
    "be reversible" in {
      val data = ImmutableMatrix(8,8, Array(-76d,-73d,-67d,-62d,-58d,-67d,-64d,-55d,
                                            -65d,-69d,-73d,-38d,-19d,-43d,-59d,-56d,
                                            -66d,-69d,-60d,-15d, 16d,-24d,-62d,-55d,
                                            -65d,-70d,-57d, -6d, 26d,-22d,-58d,-59d,
                                            -61d,-67d,-60d,-24d, -2d,-40d,-60d,-58d,
                                            -49d,-63d,-68d,-58d,-51d,-60d,-70d,-53d,
                                            -43d,-57d,-64d,-69d,-73d,-67d,-63d,-45d,
                                            -41d,-49d,-59d,-60d,-63d,-52d,-50d,-34d))
      val dct  = DiscreteCosineTransform()
      val idct = InverseDiscreteCosineTransform()
      val reconstructed = idct(dct(data))
      data zip reconstructed forall (x => x._1 must_== x._2)
    }
  }
}
