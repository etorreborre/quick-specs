package org.specs.collection
import org.specs._

class ListMultiMapSpec extends SpecificationWithJUnit {
  "a multimap allows to sort lists of elements by key" >> {
	val map = new ListMultiMap[Int, String]
	map.put(1, "a", "b") 
	map.put(2, "c", "d") 
	map(1) must_== List("a", "b")
	map(2) must_== List("c", "d")
  }
  "getting a missing key returns an empty list" >> {
   val map = new ListMultiMap[Int, String]
	map.put(1, "a", "b") 
	map(2) must_== List()
  }
}