package org.specs.quick.prune

import org.specs.SpecificationWithJUnit

import org.scalacheck.Gen
import EqualityParser._
import org.specs.matcher._
import org.specs.quick.equality._

class EqualityFlattenerSpec extends SpecificationWithJUnit with EqualityFlattener {
  noDetailedDiffs()
  "A curried equality" should {
    "not flatten Curry(a) = Curry(b)" in {
      flattenEqualities(Equality(Curry("a"), Curry("b")))(0) must be_==(Equality(Curry("a"), Curry("b")))
    }
    "flatten Apply(Curry(a), Curry(b)) = Curry(c)" in {
      flattenEqualities(Equality(Apply(Curry("a"), Curry("b")), Curry("c"))) must_== 
    	List(Equality(Curry("ab"), Apply(Curry("a"), Curry("b"))), 
    		 Equality(Curry("ab"), Curry("c")))
    }
    "flatten Apply(Apply(Curry(a), Curry(b)), Curry(c)) = Curry(d)" in {
      flattenEqualities(Equality(Apply(Apply(Curry("a"), Curry("b")), Curry("c")), Curry("d"))) must_== List(
        Equality(Curry("abc"), Apply(Curry("ab"), Curry("c"))),
        Equality(Curry("abc"), Curry("d")),
        Equality(Curry("ab"), Apply(Curry("a"), Curry("b"))),
        Equality(Curry("ab"), Curry("ab"))
        )
    }
  }
  override def newId(a: Curried) = a.show
}