package org.specs2
package quick
package classify
import expression._
import mutable._

class ClassifySpec extends Specification with ExpressionsClassifier with ExpressionsCombiner with SampleLists
  with SampleVariables {
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
	    ((s: String) => result must contain(s)).forall(expected)
	  }  
	  "return equalities for tautologies" in {
	    classify(combined).map(_.toString) must contain("[xs == xs]") 
	  }  
  }
}