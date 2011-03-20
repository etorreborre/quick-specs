package org.specs2.quick
package prune
import scala.util.parsing.combinator._
import scala.util.parsing.input._
import CurriedParser.curried
import equality._

/**
 * This parser builds equalities from string representations.
 * For example:
 * .(a, b) = c
 *
 */
private[prune] object EqualityParser extends JavaTokenParsers {
  val parser = curried ~ "=" ~ curried ^^ { case CurriedParser.~(CurriedParser.~(a,s), b) =>
    Equality(a, b) 
  }
  implicit def fromString(s: String): Equality[Curried] = parser.apply(new CharSequenceReader(s)).get
}