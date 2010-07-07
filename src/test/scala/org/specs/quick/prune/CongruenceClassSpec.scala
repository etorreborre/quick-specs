package org.specs.quick.prune
import org.specs.SpecificationWithJUnit
import org.specs.quick.equality._
import org.specs.quick.methods._
import org.specs.quick.expression._
import org.specs.quick.prune.CongruenceClass._

class CongruenceClassSpec extends SpecificationWithJUnit("A congruence class for equalities") with SampleExpressions with ExpressionCurrier { outer =>
  val congruence = new CongruenceClass {
    def println(m: Any) = outer.println(m)
    def printf(format: String, args: Any*) = outer.printf(format, args)
  }
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
