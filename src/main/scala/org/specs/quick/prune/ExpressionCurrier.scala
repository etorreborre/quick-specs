package org.specs.quick.prune
import org.specs.quick.expression._
import org.specs.quick.equality._
import org.specs.quick.classify._

/**
 * This trait curries equality expressions so that
 * every expression is an application of one parameter to a function:
 * 
 * .(wait, (const, arb, const))
 * 
 * is transformed to
 * 
 * .(.(.(wait, const), arb), const)
 */
private[prune] trait ExpressionCurrier {
  val curryfy: List[Equality[Expression]] => (List[Equality[_]], List[Equality[_]]) = curryfyEqualities _  
  
  private def curryfyEqualities(equalities: List[Equality[Expression]]): (List[Equality[_]], List[Equality[_]]) = {
	(equalities, equalities map(e => e.map(_.curryfy)) )
  }
  implicit def curryfy[T <: Expression](e: T): Curryfier[T] = new Curryfier(e)
  case class Curryfier[T <: Expression](e: T) {
	def curryfy: Curried = {
	  e match {
		case ApplicationExpression(method, Nil) => method.curryfy
		case ApplicationExpression(method, o :: Nil) => Apply(method.curryfy, o.curryfy)
		case ApplicationExpression(method, o :: others) => others.foldLeft(Apply(method.curryfy, o.curryfy)) { (res, cur) =>
		  Apply(res, cur.curryfy)
		}
		case v: Expression => Curry(v.show)
	  }
	}
  }
}
private[prune] object ExpressionCurrier extends ExpressionCurrier
