package org.specs.quick.prune
import org.specs.quick.expression._
import org.specs.quick.equality._
import org.specs.quick.classify._

/**
 * This trait
 */
private[prune] trait ExpressionCurrier {
  def curryfy(equalities: List[Equality[Expression]]): List[Equality[Curried]] = {
	equalities map { case Equality(a, b) => Equality(a.curryfy, b.curryfy) }
  }
  implicit def curryfy[T <: Expression](e: T): Curryfier[T] = new Curryfier(e)
  case class Curryfier[T <: Expression](e: T) {
	def curryfy: Curried = {
	  e match {
		case v: VariableExpression[_] => Curry(v.variable.show)
		case m: MethodExpression => Curry(m.show)
		case ApplicationExpression(method, Nil) => method.curryfy
		case ApplicationExpression(method, o :: Nil) => Apply(method.curryfy, o.curryfy)
		case ApplicationExpression(method, o :: others) => others.foldLeft(Apply(method.curryfy, o.curryfy)) { (res, cur) =>
		  Apply(res, cur.curryfy)
		}
	  }
	}
  }
}

import scala.util.parsing.combinator._
import scala.util.parsing.input._
object CurriedParser extends JavaTokenParsers {
  val application = (".(" ~> parser) ~ ("," ~> const <~ ")") ^^ { case a ~ b => 
    Apply(a, b) 
  }    
  val const = ident ^^ { s => Curry(s) }
  val parser: Parser[Curried] = application | const
  val curried = parser
  implicit def fromString(s: String): Curried = parser.apply(new CharSequenceReader(s)).get
}
