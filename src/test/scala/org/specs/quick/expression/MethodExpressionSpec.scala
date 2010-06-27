package org.specs.quick.expression
import org.specs._
import org.specs.util._
import org.specs.quick._
import org.specs.quick.methods._

class MethodExpressionSpec extends SpecificationWithJUnit with SampleLists with SampleExpressions with ScalaMethodsFactory
with DataTables {
  object TestMethods {
	def integer(i: Int) = i
	def list(l: List[Int]) = l
  }
  noDetailedDiffs
  "applying valued expressions to a method expression must return all the possible applications" >> {
	 "method"  | "params"  			| "result"               			|
	 plusPlus  !  List(xs) 			! List(List(xs, xs))     			|
	 plusPlus  !  List(xs, ys) 		! List(List(xs, xs), List(xs, ys), 
			                               List(ys, xs), List(ys, ys))	|> { (method, params, result) =>
	   MethodExpression(method).applicableParameters(params:_*) must_== result
	 }
  }
}