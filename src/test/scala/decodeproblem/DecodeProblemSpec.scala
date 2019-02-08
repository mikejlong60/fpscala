package decodeproblem

import org.scalatest._
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks

class DecodeProblemSpec extends PropSpec with Matchers with PropertyChecks {

  val n = Gen.oneOf((1 to 26).toList)
  val oneTo26 = Gen.listOf (n)

  //Your sliding thing gave me the clue I needed. Trying to use fold was too hard.
  property("test decode problem") {
   forAll(oneTo26) { xs =>
      val combined = xs.sliding(2, 1).toList.filter(xy => xy.mkString("").toInt <= 26)
      val answer = combined.size + 1
      answer should be >= (xs.size)

    }
  }
}
 
