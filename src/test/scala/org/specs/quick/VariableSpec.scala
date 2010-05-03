package org.specs.quick
import org.specs._
import org.scalacheck._

class VariableSpec extends SpecificationWithJUnit {
  include(AllVariablesSpec)
  include(ConstVariableSpec)
  include(ArbitraryVariableSpec)
}
object AllVariablesSpec extends Specification with ScalaCheck with AnyVariables {
  "There are 2 kinds of variables: arbitrary and constants\n" +
  "A variable" should {
    "have a getType method returning the class name of its type" verifies { v: Variable[Int] =>
	  v.getType contains("int") 
	}
    "have a show method returning its name - this is used in displaying the equations" verifies { v: Variable[Int] =>
      v.show == "name"      
    }
  }
}
object ConstVariableSpec extends Specification("A constant variable") with ScalaCheck with ConstVariables {
  "A constant variable should have constant values" verifies {  v: Variable[Int] =>
      v.value == 1
   }
}
object ArbitraryVariableSpec extends Specification("An arbitrary variable") with ScalaCheck with ArbitraryVariables {
  "An arbitrary variable should have random values" verifies {  v: Variable[Int] =>
    (1 to 100).toList.map(i => v.evaluate).exists(_ < 0) must beTrue
  }
}
trait GenerationParams {
  implicit val params = Gen.defaultParams
}
trait ConstVariables extends GenerationParams { 
  implicit val constVariable: Arbitrary[Variable[Int]]= Arbitrary(Constant("name")(1))
}
trait ArbitraryVariables extends GenerationParams {
  implicit val arbitraryVariable: Arbitrary[Variable[Int]]= Arbitrary(Variable[Int]("name"))
}
trait AnyVariables extends ConstVariables with ArbitraryVariables {
  implicit val anyVariable: Arbitrary[Variable[Int]]= Arbitrary(Gen.oneOf(constVariable.arbitrary, arbitraryVariable.arbitrary))
}
