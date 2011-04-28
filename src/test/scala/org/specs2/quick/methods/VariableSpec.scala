package org.specs2
package quick
package methods
import org.scalacheck._

class VariableSpec extends Specification with ScalaCheck { def is =

  "There are 2 kinds of variables: arbitrary and constants"                                                             ^
                                                                                                                        p^
  "Any variable should"                                                                                                 ^
    "have a getType method returning the class name of its type"                                                        ! anyVar.e1^
    "have a getType method returning its type name including type parameters"                                           ! anyVar.e2^
    "have a show method returning its name - this is used in displaying the equations"                                  ! anyVar.e3^
                                                                                                                        p^
  "A constant variable should"                                                                                          ^
    "have constant values"                                                                                              ! constVar.e1^
                                                                                                                        p^
  "An arbitrary variable should"                                                                                        ^
    "have random values"                                                                                                ! arbVar.e1^
                                                                                                                        end


  object anyVar extends AnyVariables {
    def e1 = check { v: Variable[Int] => v.getType must contain("Int") }
    def e2 = check { v: Variable[List[Int]] => v.getType must contain("List[Int]") }
    def e3 = check { v: Variable[Int] =>  v.show === "name" }
  }
  object constVar extends ConstVariables {
    def e1 = check {  v: Variable[Int] => v.value === 1 }
  }
  object arbVar extends ArbitraryVariables {
    def e1 = check { v: Variable[Int] => (1 to 100).toList.map(i => v.evaluate) must have(_ < 0)  }
  }
}

trait GenerationParams {
  implicit val params = Gen.Params()
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
