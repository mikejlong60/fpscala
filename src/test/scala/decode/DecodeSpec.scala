package decode

import org.scalatest._
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks

class DecodeSpec extends PropSpec with Matchers with PropertyChecks {

  property("dropwhile should drop some even ints") {

    val a = new Decoder()
    a.size(List("1", "2", "3")) shouldBe 5
  }
}
