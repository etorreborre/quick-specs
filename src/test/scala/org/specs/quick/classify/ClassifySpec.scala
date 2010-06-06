package org.specs.quick.classify
import org.specs.quick.expression._
import org.specs.quick._
import org.specs._

class ClassifySpec extends SpecificationWithJUnit with ExpressionsClassifier with ExpressionsCombiner with SampleLists with Sugar {
  noDetailedDiffs()
  val combined = combine((plusPlusAndNil, List(xs)))
  "classifying expressions" should {
	"evaluate expressions to separate them to equalities" in {
	  classify(combined).map(_.toString).toSet must containAll( 
	 	  Set("[xs == ++(nil(), xs)]",
              "[nil() == ++(nil(), nil())]",
              "[++(xs, xs) == ++(++(xs, xs), nil())]"))
	}  
	"return no equalities for tautologies" in {
	  classify(combined).map(_.toString) must not contain("[xs == xs]") 
	}  
  }
}