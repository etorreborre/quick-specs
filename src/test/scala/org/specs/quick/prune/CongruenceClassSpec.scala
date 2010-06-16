package org.specs.quick.prune
import org.specs.SpecificationWithJUnit
import org.specs.quick.equality._
import org.specs.quick.expression._

class CongruenceClassSpec extends SpecificationWithJUnit("A congruence class for equalities") with ConstantExpressions {
  val congruence = new CongruenceClass
  "it" should {
	"accept a new equality" in {
	  congruence.add(Equality("a","b"))
	  congruence.add(Equality("c","d"))

	  congruence.isCongruent("a", "b") aka "a is congruent to b" must beTrue
	  congruence.isCongruent("a", "c") aka "a is congruent to c" must beFalse
	}
  } 	
}
trait ConstantExpressions {
  implicit def exp(name: String): Expression = ConstantExpression(name)
  case class ConstantExpression(name: String) extends Expression {
    def getType = name
  }
}