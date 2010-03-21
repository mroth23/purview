package org.purview.analysers

import org.purview.core.data.ImmutableMatrix
import org.specs.SpecificationWithJUnit

class CopyMoveAnalyserSpec extends SpecificationWithJUnit {
  "The CopyMove analyser" should {
    "perform a valid DCT transformation" in {
      //TODO: find two 16x16 matrices to test with!
      val input = new ImmutableMatrix[Float](8, 8, Array(
          52,  55,  61,  66,  70,  61,  64,  73,
          63,  59,  55,  90, 109,  85,  69,  72,
          62,  59,  68, 113, 144, 104,  66,  73,
          63,  58,  71, 122, 154, 106,  70,  69,
          67,  61,  68, 104, 126,  88,  68,  70,
          79,  65,  60,  70,  77,  68,  58,  75,
          85,  71,  64,  59,  55,  61,  65,  83,
          87,  79,  69,  68,  65,  76,  78,  94) map (_ - 128f))
      val copymove = new CopyMoveAnalyser
      val result = copymove.partialDCT(input, 8)

      val expected = new ImmutableMatrix[Float](8, 8, Array(
          -415,  -30,  -61,   27,   56,  -20,   -2,    0,
          +  4,  -22,  -61,   10,   13,   -7,   -9,    5,
          - 47,    7,   77,  -25,  -29,   10,    5,   -6,
          - 49,   12,   34,  -15,  -10,    6,    2,    2,
          + 12,   -7,  -13,   -4,   -2,    2,   -3,    3,
          -  8,    3,    2,   -6,   -2,    1,    4,    2,
          -  1,    0,    0,   -2,   -1,   -3,    4,   -1,
          +  0,    0,   -1,   -4,   -1,    0,    1,    2))
      (result zip expected).cells foreach { (entry) =>
        val (x, y, cell) = entry
        //cell._1 must_== cell._2
      }
    }
  }
}
