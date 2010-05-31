package org.specs.quick.prune
import org.specs.quick.expression._
import org.specs.quick.classify._

trait ExpressionCurrier extends CurriedExpressions {
  implicit def toCurry[T <: Expression](e: T): ToCurry[T] = new ToCurry(e)
  case class ToCurry[T <: Expression](e: T) {
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
  def curryfy(classes: List[EquivalenceClass]): List[Equality[Curried]] = {
	classes.foldLeft(List[Equality[Curried]]()) { (res, cur) => 
	  cur.equalities.map(e => new CurriedEquality(e.a.curryfy, e.b.curryfy)) ::: res
	}
  }
}
import scala.util.parsing.combinator._
import scala.util.parsing.input._
  trait Curried
  case class Apply(a: Any, b: Curried) extends Curried {
	override def toString = List(a, b).mkString(".(", ", ", ")") 
  }
  case class Curry(a: Any) extends Curried {
	override def toString = a.toString 
  }

trait CurriedExpressions {
  case class CurriedEquality(a: Curried, b: Curried) extends Equality(a, b) {
	def curryfy = this
  }
  object CurriedParser extends JavaTokenParsers {
    val application = (".(" ~> parser) ~ ("," ~> const <~ ")") ^^ { case a ~ b => 
      Apply(a, b) 
    }    
    val const = ident ^^ { s => Curry(s) }
    val parser: Parser[Curried] = application | const
    val curried = parser
    implicit def fromString(s: String): Curried = parser.apply(new CharSequenceReader(s)).get
  }
}