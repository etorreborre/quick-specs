package org.specs2
package collection
import mutable._
import quick._
import quick.methods._


class ListMultiMapSpec extends Specification {
  val map = new ListMultiMap[Int, String]
  "a multimap allows to sort lists of elements by key" >> {
	  map.put(1, "a", "b") 
	  map.put(2, "c", "d") 
	  map(1) must_== List("a", "b")
	  map(2) must_== List("c", "d")
  }
  "getting a missing key returns an empty list" >> {
	  map.put(1, "a", "b") 
	  map(2) must_== List()
  }
  "toString prints out keys and elements k -> [1, 2, 3]" >> {
	  map.put(1, "a", "b") 
	  map.put(2, "c", "d") 
	  map.toString must be_==("Map(1 -> [a, b], 2 -> [c, d])") or be_==("Map(2 -> [c, d], 1 -> [a, b])") 
  }
}
import org.scalacheck._
class ListMultiMapSpecs extends QuickSpecs with Specification {
  implicit val strings: Arbitrary[String] = Arbitrary(Gen.oneOf("a", "b", "c"))
  "specs for the put method" in {
    quick(new ListMultiMap[Int, String].select("put[Int, String]", "apply[Int]").functions :+ 
    	  new ScalaTupledFunction("list", (s: String) => List(s)), 
    		freshConstant("map", new ListMultiMap[Int, String]), 
    		variable[Int]("i"), 
      		variable[String]("s")) must_== "[list(s) == map.put(i, s).apply(i)]"
  }
}
