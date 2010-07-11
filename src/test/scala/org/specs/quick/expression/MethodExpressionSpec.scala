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
  val ls = VariableExpression(constant(Lists))
  noDetailedDiffs
  "applying valued expressions to a method expression must return all the possible applications" >> {
	 "method"  | "params"  			| "result"               					|
	 listsName !  List(ls) 			! List(List(ls))		     				|
	 plusPlus  !  List(ls, xs) 		! List(List(ls,xs, xs))     				|
	 plusPlus  !  List(ls, xs, ys) 	! List(List(ls, xs, xs), List(ls, xs, ys), 
			                               List(ls, ys, xs), List(ls, ys, ys))	|> { (method, params, result) =>
	   MethodExpression(method).applicableParameters(params:_*) must_== result
	 }
  }
  "With a method accepting generic parameters" >> {
	val map = new ListMultiMap[Int, String]
    val put: ScalaMethod = map.select("put[Int, String]").methods(0)
    val i: VariableExpression[Int] = variable[Int]("i")
    val s: VariableExpression[String] = variable[String]("s")
    val m = MethodExpression(put)
    m.applicableParameters(constant(map), i, s) must have size(1) and be_==("List(Map(), i, s)") ^^ (_.apply(0).toString)
  }
}