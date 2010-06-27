package org.specs.quick.methods
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
	  v.getType contains "Int" 
	}
    "have a getType method returning its full type name" verifies { v: Variable[List[Int]] =>
	  v.getType contains "List[Int]" 
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
  implicit val constListIntVariable: Arbitrary[Variable[List[Int]]]= Arbitrary(Constant("list(1)")(List(1)))
  implicit val constIntVariable: Arbitrary[Variable[Int]]= Arbitrary(Constant("name")(1))
}
trait ArbitraryVariables extends GenerationParams {
  implicit val arbitraryListIntVariable: Arbitrary[Variable[List[Int]]]= Arbitrary(Variable[List[Int]]("list(1)"))
  implicit val arbitraryIntVariable: Arbitrary[Variable[Int]]= Arbitrary(Variable[Int]("name"))
}
trait AnyVariables extends ConstVariables with ArbitraryVariables {
  implicit val anyListIntVariable: Arbitrary[Variable[List[Int]]]= Arbitrary(
		  Gen.oneOf(constListIntVariable.arbitrary, arbitraryListIntVariable.arbitrary))
  implicit val anyIntVariable: Arbitrary[Variable[Int]]= Arbitrary(
		  Gen.oneOf(constIntVariable.arbitrary, arbitraryIntVariable.arbitrary))
}
