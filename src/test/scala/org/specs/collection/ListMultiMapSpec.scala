package org.specs.collection
import org.specs._
import org.specs.quick._
import org.specs.quick.methods._

class ListMultiMapSpec extends SpecificationWithJUnit with Sugar {
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
class ListMultiMapSpecs extends Specification with QuickSpecs with Sugar {
  implicit val strings = Arbitrary.arbitrary[Array[String]]
  "specs for the put method" in {
	level = Debug
    quick(new ListMultiMap[Int, String].select("put[Int, String]", "apply[Int]").pp.functions.pp :+ 
    	  new ScalaTupledFunction("list", (s: String) => List(s)), 
    		constant("map", new ListMultiMap[Int, String]), 
    		variable[Int]("i"), 
      		variable[String]("s")).pp.isExpectation
  }
}
