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
    "not flatten Apply(Curry(a), Curry(b)) = Curry(c)" in {
      flattenEqualities(Equality(Apply(Curry("a"), Curry("b")), Curry("c"))) must_== 
    	List(Equality(Apply(Curry("a"), Curry("b")), Curry("c")))
    }
    "flatten Apply(Apply(Curry(a), Curry(b)), Curry(c)) = Curry(d)" in {
      flattenEqualities(Equality(Apply(Apply(Curry("a"), Curry("b")), Curry("c")), Curry("d"))) must_== List(
        Equality(Apply(Curry("ab"), Curry("c")), Curry("d")),
        Equality(Apply(Curry("a"), Curry("b")), Curry("ab")))
    }
  }
  override def newId(a: Curried) = a.show
}