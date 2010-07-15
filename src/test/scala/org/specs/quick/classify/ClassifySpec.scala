package org.specs.quick.classify
import org.specs.quick.expression._
import org.specs.quick._
import org.specs._

class ClassifySpec extends SpecificationWithJUnit with ExpressionsClassifier with ExpressionsCombiner with SampleLists 
  with SampleVariables with Sugar {
  noDetailedDiffs()
  override val partitionsNumber = 4
  combineDepth(2)
  
  val combined = combine((plusPlusAndNil.methods, List(lists, xs)))
  "classifying expressions" should {
	"evaluate expressions to separate them to equalities" in {
	  val expected = List(
	 	  "[xs == xs]",
	 	  "[xs == lists.++(lists.nil(), xs)]",
	 	  "[xs == lists.++(xs, lists.nil())]",
     	  "[lists.nil() == lists.++(lists.nil(), lists.nil())]")
	  val result = classify(combined).map(_.toString) 
	  expected foreach (result must contain(_))
	}  
	"return equalities for tautologies" in {
	  classify(combined).map(_.toString) must contain("[xs == xs]") 
	}  
  }
}