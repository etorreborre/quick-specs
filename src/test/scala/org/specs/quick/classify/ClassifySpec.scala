package org.specs.quick.classify
import org.specs.quick.expression._
import org.specs.quick._
import org.specs._

class ClassifySpec extends SpecificationWithJUnit with ExpressionsClassifier with ExpressionsCombiner with SampleLists 
  with SampleVariables with Sugar {
  noDetailedDiffs()
  override val partitionsNumber = 4
  combineDepth(1)
  
  val combined = combine((plusPlusAndNil, List(xs)))
  "classifying expressions" should {
	"evaluate expressions to separate them to equalities" in {
	  classify(combined).map(_.toString).mkString("\n") must_== List( 
	 	  "[xs == xs]",
	 	  "[xs == ++(nil(), xs)]",
	 	  "[xs == ++(xs, nil())]",
     	  "[nil() == ++(nil(), nil())]").mkString("\n")
	}  
	"return equalities for tautologies" in {
	  classify(combined).map(_.toString) must contain("[xs == xs]") 
	}  
  }
}