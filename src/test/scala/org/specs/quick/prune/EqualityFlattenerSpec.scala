package org.specs.quick.prune

import org.specs.SpecificationWithJUnit
import org.scalacheck.Gen
import EqualityParser._

class EqualityFlattenerSpec extends SpecificationWithJUnit with EqualityFlattener {
  "A curried equality" should {
    "be flattened as a list of equalities" in {
      noDetailedDiffs()
      // .(.(a, b), c) = d => .(ab, c) = d; .(a, b) = ab 
      flattenEqualities(fromString(".(.(a, b), c) = d")).toString must_== List(fromString(".(ab, c) = d"), fromString(".(a, b) = ab")).toString    	
    }
  }
}