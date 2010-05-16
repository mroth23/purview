package org.purview.core.transforms

import org.purview.core.data.ImmutableMatrix
import org.specs.SpecificationWithJUnit

class DCTSpec extends SpecificationWithJUnit {
  "A discrete cosine transform" should {
    "be reversible" in {
      val data = ImmutableMatrix(4,4, Array.fill(16)(util.Random.nextInt(255) - 128f))
      val dct  = DiscreteCosineTransform()
      val idct = InverseDiscreteCosineTransform()
      val reconstructed = idct(dct(data))
      data zip reconstructed forall (x => x._1 must_== x._2)
    }
  }
}
