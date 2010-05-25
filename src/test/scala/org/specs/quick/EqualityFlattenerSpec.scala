package org.specs.quick

import org.specs.SpecificationWithJUnit
import org.scalacheck.Gen

class EqualityFlattenerSpec extends SpecificationWithJUnit with EqualityFlattener with CurriedExpressions {
  import EqualityParser._
  "A curried equality" should {
    "be flattened as a list of equalities" in {
      noDetailedDiffs()
      // .(.(a, b), c) = d => .(ab, c) = d; .(a, b) = ab 
      flatten(fromString(".(.(a, b), c) = d")).toString must_== List(fromString(".(ab, c) = d"), fromString(".(a, b) = ab")).toString    	
    }
  }
}