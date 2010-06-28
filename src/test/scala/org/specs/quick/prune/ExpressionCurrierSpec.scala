package org.specs.quick.prune

import org.specs.quick.methods._
import org.specs.quick.expression._
import org.scalacheck.Gen

import org.specs.SpecificationWithJUnit

class ExpressionCurrierSpec extends SpecificationWithJUnit with ExpressionCurrier {
  import CurriedParser._
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
    constVariableExp.curryfy.toString must_== "Curry("+constVariableExp.toString+")"
  }
  val m = ScalaMethods.create(new Object).methods(0)
  "A method expression should be curried with just its name" in {
    MethodExpression(m).curryfy must_== Curry("wait")
  }
  "A composed expression" should {
    val m = ScalaMethods.create(new Object).methods(0)
    "be curried with just its name when it has no parameter" in {
      ApplicationExpression(MethodExpression(m), Nil).curryfy must_== Curry("wait")
    }
    "be curried as an application when there is one parameter" in {
      ApplicationExpression(MethodExpression(m), constVariableExp :: Nil).curryfy must_== Apply(Curry("wait"), Curry("const"))
    }
    "be curried with 2 successive applications of one parameter when there are 2 parameters" in {
      ApplicationExpression(MethodExpression(m), constVariableExp :: arbVariableExp :: Nil).curryfy must_==
    	  fromString(".(.(wait, const), arb)")
    }
    "only be curried with 2 successive applications of one parameter when there are 2 parameters - show" in {
      ApplicationExpression(MethodExpression(m), constVariableExp :: arbVariableExp :: Nil).curryfy must_==
    	  Apply(Apply(Curry("wait"), Curry("const")), Curry("arb"))
    }
    "be curried with 3 successive applications of one parameter when there are 3 parameters" in {
      ApplicationExpression(MethodExpression(m), constVariableExp :: arbVariableExp :: constVariableExp :: Nil).curryfy must_==
    	  fromString(".(.(.(wait, const), arb), const)")
    }
    "be curried with 3 successive applications of one parameter when there are 2 applications" in {
       val firstApplication = ApplicationExpression(MethodExpression(m), constVariableExp :: arbVariableExp :: Nil)
    	ApplicationExpression(MethodExpression(m), firstApplication :: arbVariableExp :: Nil).curryfy must_==
    	  fromString(".(.(wait, .(.(wait, const), arb)), arb)")
    }
  }
}

