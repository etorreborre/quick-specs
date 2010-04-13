package org.specs.quick
import org.specs._
import org.scalacheck._

class VariableSpec extends SpecificationWithJUnit with ScalaCheck {
  implicit val params = Gen.defaultParams
  val v = new Variable[Int]("v")
  "A variable" should {
    "have the type of its erasure" in {
      v.getType must_==/ "Int"
    }
    "be represented with its name" in {
      v.toString must_== "Variable(v)"      
    }
    "have random values" in {
      (1 to 10).toList.map(i => v.value).flatMap(x => x).exists(_ < 0) must beTrue 
    }
  }
}
