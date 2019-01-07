package chapter1

import org.scalatest._

class CubeCalculatorTest extends FlatSpec with Matchers {
  "cubecalculator" should "cube" in {
    CubeCalculator.cube(3) shouldBe 27
  }
}
