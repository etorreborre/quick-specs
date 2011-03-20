package org.specs2
package quick
package prune
import mutable.Specification
import equality._
import methods._
import expression._
import prune.CongruenceClass._

class CongruenceClassSpec extends Specification with SampleExpressions with ExpressionCurrier { outer =>
  "A congruence class for equalities".title
  val congruence = new CongruenceClass {}

  "adding constants to the congruence relation" should {
	  "extends the representative map" in {
	    "with a simple equality" >> {
	      congruence.add(Equality(Curry("a"), Curry("b")))
	      congruence.representative must havePair(Curry("a") -> Curry("b"))
	    }
	    "with 2 transitive equalities" >> {
	      congruence.add(Equality(Curry("a"), Curry("b")))
	      congruence.add(Equality(Curry("b"), Curry("c")))
	      congruence.representative must havePair(Curry("a") -> Curry("b")) and 
	                                     havePair(Curry("b") -> Curry("b")) and
	                                     havePair(Curry("c") -> Curry("b"))
	    }
	    "with an application" >> {
	      congruence.add(Equality(Apply(Curry("a"), Curry("b")), Curry("c")))
	      congruence.representative must havePair(Curry("ab") -> Curry("c")) 
	    }
	  }
  }
  "a congruence relationship" should {
	  "accept equalities with an application and check congruence for a symetric equality" in {
	    val eq1: Equality[Expression] = Equality(apply("+", xs, nil), xs)
	    val eq2: Equality[Expression] = Equality(xs, apply("+", xs, nil))
	  
	    congruence.add(eq1)

	    congruence.isCongruent(eq1) aka (eq1+" must be already registered") must beTrue
	    congruence.isCongruent(eq2) aka (eq2+" must be already registered") must beTrue
	  }
	  "refuse the congruence of 2 equations differing by type variables" in {
	    val eq1: Equality[Expression] = Equality(apply("+", xs, nil), xs)
	    val eq2: Equality[Expression] = Equality(apply("+", ints, nilInts), ints)
	  
	    congruence.add(eq1)

	    congruence.isCongruent(eq1) aka (eq1+" must be already registered") must beTrue
	    congruence.isCongruent(eq2) aka (eq2+" must not be already registered") must beFalse
	  }
  } 	
}
