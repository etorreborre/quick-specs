package org.specs.quick

trait ExpressionCurrier {
  implicit def toCurry(e: Expression) = new ToCurry(e)
  class ToCurry(e: Expression) {
	def curryfy: Curried = {
	  e match {
		case v: VariableExpression[_] => Curry(v.variable.show)
		case m: MethodExpression => Curry(m.show)
		case ComposedExpression(m, Nil) => m.curryfy
		case ComposedExpression(m, o :: Nil) => Apply(m.curryfy, o.curryfy)
		case ComposedExpression(m, o :: others) => others.foldLeft(Apply(m.curryfy, o.curryfy)) { (res, cur) =>
		  Apply(res, cur.curryfy)
		}
	  }
	}
  }
  trait Curried
  case class Apply(c: Any, a: Curried) extends Curried
  case class Curry(a: Any) extends Curried
  def curryfy(classes: List[EquivalenceClass]) = {
	classes.foldLeft(List[Curried]()) { (res, cur) => 
	  cur.expressions.map(_.curryfy) ::: res
	}
  }

}