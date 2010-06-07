package org.specs.collection
import org.specs._

class ListMultiMapSpec extends SpecificationWithJUnit {
  val map = new ListMultiMap[Int, String]
  "a multimap allows to sort lists of elements by key" >> {
	map.put(1, "a", "b") 
	map.put(2, "c", "d") 
	map(1) must_== List("a", "b")
	map(2) must_== List("c", "d")
  }
}