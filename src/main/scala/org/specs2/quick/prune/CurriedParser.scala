package org.specs2.quick
package prune
import scala.util.parsing.combinator._
import scala.util.parsing.input._

/**
 * this parser is used to buid curried expressions from a string representation
 * 
 * .(.(.(wait, const), arb), const) will be parsed as successive applications
 *
 */
private[prune] object CurriedParser extends JavaTokenParsers {
  val application = (".(" ~> parser) ~ ("," ~> parser <~ ")") ^^ { case a ~ b => 
    Apply(a, b) 
  }    
  val const = ident ^^ { s => Curry(s) }
  val parser: Parser[Curried] = application | const
  val curried = parser
  implicit def fromString(s: String): Curried = parser.apply(new CharSequenceReader(s)).get
}
