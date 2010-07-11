package org.specs.collection
import org.specs._
import org.specs.quick._

class ListMultiMapSpec extends SpecificationWithJUnit {
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
  //level = Debug
  implicit val strings = Arbitrary.arbitrary[Array[String]]
  object ListMultiMapExample {
	def put(m: ListMultiMap[Int, String], i: Int, s: String): ListMultiMap[Int, String] = { 
	  m.put(i, s)
	  m
    }
	def get(m: ListMultiMap[Int, String], i: Int): List[String] = m.apply(i)
  }
  "specs for the put method" in {
	level = Debug
//    quick(ListMultiMapExample.accept("put", "get"), 
//    		constant("map", new ListMultiMap[Int, String]), variable[Int]("i"), variable[String]("s")).pp.isExpectation
    quick(new ListMultiMap[Int, String].select("put[Int, java.lang.String]", "apply[Int]"), 
    		constant("map", new ListMultiMap[Int, String]), 
    		variable[Int]("i"), 
    		variable[String]("s")).pp.isExpectation
  }
}
