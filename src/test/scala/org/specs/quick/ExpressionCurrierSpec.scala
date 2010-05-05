package org.specs.quick

import org.specs.SpecificationWithJUnit
import org.scalacheck.Gen

class ExpressionCurrierSpec extends SpecificationWithJUnit with ExpressionCurrier {
  implicit val params = Gen.defaultParams
  val arbVariable = Variable[Int]("arb")
  val constVariable = Constant("const")(1)
  val arbVariableExp = VariableExpression(arbVariable)
  val constVariableExp = VariableExpression(constVariable)
  
  "An arbitrary variable expression should be curried with just its name" in {
    arbVariableExp.curryfy must_== Curry("arb")
  }
  "A constant variable expression should be curried with just its name" in {
    constVariableExp.curryfy must_== Curry("const")
  }
  val m = ScalaMethods.create(new Object).methods(0)
  "A method expression should be curried with just its name" in {
    MethodExpression(m).curryfy must_== Curry("wait")
  }
  "A composed expression" should {
    val m = ScalaMethods.create(new Object).methods(0)
    "be curried with just its name when it has no parameter" in {
      ComposedExpression(MethodExpression(m), Nil).curryfy must_== Curry("wait")
    }
    "be an application when there is one parameter" in {
      ComposedExpression(MethodExpression(m), constVariableExp :: Nil).curryfy must_== Apply(Curry("wait"), Curry("const"))
    }
    "be an successive applications of one parameter when there are 2 parameters" in {
      ComposedExpression(MethodExpression(m), constVariableExp :: arbVariableExp :: Nil).curryfy.toString must_== 
    	  ".(.(wait, const), arb)"
    }
  }
}