package org.specs.quick

import org.specs.SpecificationWithJUnit
import org.scalacheck.Gen

class EqualityFlattenerSpec extends SpecificationWithJUnit with EqualityFlattener with CurriedExpressions {
  "A curried equality" should {
    "be flattened as a list of equalities" in {
    }
  }
}