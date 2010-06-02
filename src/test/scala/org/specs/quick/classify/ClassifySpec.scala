package org.specs.quick.classify
import org.specs.quick.expression._
import org.specs.quick._
import org.specs._

class ClassifySpec extends SpecificationWithJUnit with ExpressionsClassifier with ExpressionsCombiner with SampleLists {
  "classifying expressions" should {
	"evaluate expressions to separate them to equalities" in {
	  classify(combine((Lists.accept("\\+\\+", "nil"), List(xs))))	
	}  
  }
}