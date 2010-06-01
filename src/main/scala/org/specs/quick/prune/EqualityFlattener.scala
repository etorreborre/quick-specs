package org.specs.quick.prune
import org.specs.quick.classify._
import org.specs.quick.equality._

private[prune] trait EqualityFlattener { 
  val flatten = flattenEqualitiesList _
  
  def flattenEqualities(curried: Equality[_]*): List[Equality[_]] = flattenEqualitiesList(curried.toList)
  def flattenEqualitiesList(curried: List[Equality[_]]): List[Equality[_]] = {
	curried.foldLeft(Nil:List[Equality[_]]) { (res: List[Equality[_]], cur: Equality[_]) =>
	  flattenCurried(cur) ::: res
	}
  }
  private def flattenCurried(curried: Equality[_]): List[Equality[_]] = {
	curried match {
	  case Equality(Curry(a), Curry(b)) => 
	    List(Equality(Curry(a), Curry(b)))
	  case Equality(Apply(Apply(a, b), c), d) => {
	    val ab = a.toString + b.toString
	    Equality(Apply(ab, c), d) :: flattenCurried(Equality(Apply(a, b), Curry(ab)))
	  }
	  case Equality(Apply(a, Curry(b)), c) => List(Equality(Apply(a, Curry(b)), c))
	  case Equality(a, b) if (a == b) => Nil
	  case Equality(a, b) if (a != b) => flattenCurried(Equality(b, a))
    }
  }
  
  import scala.util.parsing.combinator._
  import scala.util.parsing.input._
  import CurriedParser.curried
  object EqualityParser extends JavaTokenParsers {
    val parser = curried ~ "=" ~ curried ^^ { case CurriedParser.~(CurriedParser.~(a,s), b) =>
      Equality(a, b) 
    }
    implicit def fromString(s: String): Equality[_] = parser.apply(new CharSequenceReader(s)).get
  }
}
private[prune] object EqualityFlattener extends EqualityFlattener 


