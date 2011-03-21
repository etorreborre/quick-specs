package org.specs2
package quick
package prune
import mutable.Specification

import org.scalacheck.Gen
import equality._
import matcher._

class EqualityFlattenerSpec extends Specification with EqualityFlattener with DataTables {
  "new Curry elements can be created from others" >> {
  	"element"   									                     | "result"     |
  	 (Curry("a"):Curried)  									           ! Curry("a")   |
  	 Apply(Curry("a"), Curry("b"))                     ! Curry("ab")  |
	   Apply(Apply(Curry("a"), Curry("b")), Curry("c"))  ! Curry("abc") |> { (element, result) =>
      newCurry(element) must_== result
	  }
  }
  "A curried equality" should {
    "not flatten Curry(a) = Curry(b)" in {
      flattenEqualities(Equality(Curry("a"), Curry("b")))(0) must be_==(Equality(Curry("a"), Curry("b")))
    }
    "flatten Apply(Curry(a), Curry(b)) = Curry(c)" in {
      flattenEqualities(Equality(Apply(Curry("a"), Curry("b")), Curry("c"))) must_== 
    	List(Equality(Curry("ab"), Curry("c")),
    	     Equality(Apply(Curry("a"), Curry("b")), Curry("ab")))
    }
    "flatten Apply(Apply(Curry(a), Curry(b)), Curry(c)) = Curry(d)" in {
      flattenEqualities(Equality(Apply(Apply(Curry("a"), Curry("b")), Curry("c")), Curry("d"))) must_== List(
        Equality(Curry("abc"), Curry("d")),
        Equality(Apply(Curry("ab"), Curry("c")), Curry("abc")),
        Equality(Curry("ab"), Curry("ab")),
        Equality(Apply(Curry("a"), Curry("b")), Curry("ab"))
        )
    }
    "flatten Curry(d) = Apply(Apply(Curry(a), Curry(b)), Curry(c))" in {
      flattenEqualities(Equality(Curry("d"), Apply(Apply(Curry("a"), Curry("b")), Curry("c")))) must_== List(
        Equality(Curry("abc"), Curry("d")),
        Equality(Apply(Curry("ab"), Curry("c")), Curry("abc")),
        Equality(Curry("ab"), Curry("ab")),
        Equality(Apply(Curry("a"), Curry("b")), Curry("ab"))
        )
    }
    "flatten Apply(Apply(Curry(a), Curry(b)), Curry(c)) = Apply(Curry(d), Curry(e))" in {
      flattenEqualities(Equality(Apply(Apply(Curry("a"), Curry("b")), Curry("c")), Apply(Curry("d"), Curry("e")))) must_== List(
    	Equality(Curry("abc"), Curry("de")),
        Equality(Curry("abc"), Curry("abc")),
        Equality(Apply(Curry("ab"), Curry("c")), Curry("abc")),
        Equality(Curry("de"), Curry("de")),
        Equality(Apply(Curry("d"), Curry("e")), Curry("de")),
        Equality(Curry("ab"), Curry("ab")),
        Equality(Apply(Curry("a"), Curry("b")), Curry("ab"))
        )
    }
    "flatten Apply(Curry(d), Curry(e)) = Apply(Apply(Curry(a), Curry(b)), Curry(c))" in {
      flattenEqualities(Equality(Apply(Curry("d"), Curry("e")), Apply(Apply(Curry("a"), Curry("b")), Curry("c")))) must_== List(
    	Equality(Curry("de"), Curry("abc")),
    	Equality(Curry("de"), Curry("de")),
        Equality(Apply(Curry("d"), Curry("e")), Curry("de")),
    	Equality(Curry("abc"), Curry("abc")),
        Equality(Apply(Curry("ab"), Curry("c")), Curry("abc")),
        Equality(Curry("ab"), Curry("ab")),
        Equality(Apply(Curry("a"), Curry("b")), Curry("ab"))
        )
    }
  }
  override def newId(a: Curried) = a.show
}