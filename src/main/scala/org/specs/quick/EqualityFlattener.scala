package org.specs.quick

trait EqualityFlattener { this: CurriedExpressions =>
  def flatten(curried: Equality[_]*): List[Equality[_]] = flatten(curried.toList)
  def flatten(curried: List[Equality[_]]): List[Equality[_]] = curried
  
  import scala.util.parsing.combinator._
  import scala.util.parsing.input._

  object EqualityParser {
    val parser = (CurriedParser.parser ~ " = " ~ CurriedParser.parser) ^^ { case x =>
    println(x)
    x match {
    	case CurriedParser.~(a, b) => new Equality(a, b) 
    }
      
    }
    implicit def fromString(s: String): Equality[_] = parser.apply(new CharSequenceReader(s)).get
  }
}

