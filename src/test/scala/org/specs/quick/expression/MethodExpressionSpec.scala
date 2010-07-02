package org.specs.quick.expression
import org.specs._
import org.specs.util._
import org.specs.quick._
import org.specs.quick.methods._
import org.scalacheck._
import org.scalacheck.Gen.Params
import org.specs.collection._

class MethodExpressionSpec extends SpecificationWithJUnit with SampleLists with SampleExpressions with ScalaMethodsFactory
with DataTables with DefaultParams {

  noDetailedDiffs
  "applying valued expressions to a method expression must return all the possible applications" >> {
	 "method"  | "params"  			| "result"               			|
	 plusPlus  !  List(xs) 			! List(List(xs, xs))     			|
	 plusPlus  !  List(xs, ys) 		! List(List(xs, xs), List(xs, ys), 
			                               List(ys, xs), List(ys, ys))	|> { (method, params, result) =>
	   MethodExpression(method).applicableParameters(params:_*) must_== result
	 }
  }
  "With generic method" >> {
    val put: ScalaMethod = new ListMultiMap[Int, String].accept("put").get("put")
    val i: VariableExpression[Int] = variable[Int]("i")
    val s: VariableExpression[String] = variable[String]("s")
    val m = MethodExpression(put)
    m.applicableParameters(i, s) must_== List(List(i, i), List(i, s), List(s, i), List(s, s))
  }
}