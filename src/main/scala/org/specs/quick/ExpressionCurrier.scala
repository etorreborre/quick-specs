package org.specs.quick

trait ExpressionCurrier extends CurriedExpressions {
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
  def curryfy(classes: List[EquivalenceClass]): List[Equality] = {
	classes.foldLeft(List[Equality]()) { (res, cur) => 
	  cur.equalities.map(_.curryfy) ::: res
	}
  }
}
import scala.util.parsing.combinator.syntactical.TokenParsers
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.token._
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.input._

trait CurriedExpressions {
  trait Curried
  case class Apply(c: Any, a: Curried) extends Curried {
	override def toString = List(c, a).mkString(".(", ", ", ")") 
  }
  case class Curry(a: Any) extends Curried {
	override def toString = a.toString 
  }
  case class CurriedEquality(a: Curried, b: Curried) extends Equality {
	def curryfy = this
  }
  object CurriedParser extends JavaTokenParsers {
    val application = (".(" ~> parser) ~ (", " ~> const <~ ")") ^^ { case a ~ b => 
      Apply(a, b) 
    }    
    val const = ident ^^ { s => Curry(s) }
    val parser: Parser[Curried] = application | const
    implicit def fromString(s: String): Curried = parser.apply(new CharSequenceReader(s)).get
  }
}