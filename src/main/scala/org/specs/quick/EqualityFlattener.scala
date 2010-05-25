package org.specs.quick

trait EqualityFlattener { this: CurriedExpressions =>
  def flatten(curried: Equality[_]*): List[Equality[_]] = flatten(curried.toList)
  def flatten(curried: List[Equality[_]]): List[Equality[_]] = {
	curried.foldLeft(Nil:List[Equality[_]]) { (res: List[Equality[_]], cur: Equality[_]) =>
	  flattenCurried(cur) ::: res
	}
  }
  private def flattenCurried(curried: Equality[_]): List[Equality[_]] = curried match {
	case CurriedEquality(Curry(a), Curry(b)) => 
	   List(CurriedEquality(Curry(a), Curry(b)))
	case CurriedEquality(Apply(Apply(a, b), c), d) => {
	  val ab = a.toString + b.toString
	  CurriedEquality(Apply(ab, c), d) :: flattenCurried(CurriedEquality(Apply(a, b), Curry(ab)))
	}
	case CurriedEquality(Apply(a, Curry(b)), c) => 
	   List(CurriedEquality(Apply(a, Curry(b)), c))
  }
  
  import scala.util.parsing.combinator._
  import scala.util.parsing.input._
  import CurriedParser.curried
  object EqualityParser extends JavaTokenParsers {
    val parser = curried ~ "=" ~ curried ^^ { case CurriedParser.~(CurriedParser.~(a,s), b) =>
      CurriedEquality(a, b) 
    }
    implicit def fromString(s: String): Equality[_] = parser.apply(new CharSequenceReader(s)).get
  }
}

