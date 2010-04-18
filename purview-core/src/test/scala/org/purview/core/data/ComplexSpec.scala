package org.purview.core.data

import org.specs.SpecificationWithJUnit
import org.purview.core.data.Complex._

class ComplexSpec extends SpecificationWithJUnit {
  "A complex number" should {
    "be constructable using any type" in {
      val cInt = Complex(1, 2)
      println(cInt)
      val cDouble = Complex(0.1, 0.2)
      println(cDouble)
      val cFloat = Complex(0.1f, 0.3f)
      println(cFloat)
      val cChar = Complex('a', 'b')
      println(cChar)
    }

    "support basic addition" in {
      val c1 = Complex(1, 2)
      val c2 = Complex(2, 3)
      val c3 = c1 + c2
      c3 must_== Complex(3, 5)
    }

    "be used in any Numeric monoid position" in {
      import util.Random
      val complexes = for(i <- 0 to 10) yield Complex(Random.nextDouble, Random.nextDouble)
      val result = complexes.sum * complexes.product
      println(result)
    }

    "support implicit conversions from real numbers" in {
      val x = 1.i + 2 - 1.i
      x must_== Complex(2, 0)

      val y = 2.0.i + 1.0 + 1.0
      y must_== Complex(2.0, 2.0)
    }

    "support tuple arithmetic" in {
      val c = Complex(5, 6)
      val tuple = (2, 3)
      val result = c + tuple + (4, 7)
      result must_== Complex(11, 16)
    }
  }
}
