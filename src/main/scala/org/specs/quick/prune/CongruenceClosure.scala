package org.specs.quick.prune
import org.specs.quick.equality._
import scala.collection.mutable._
trait CongruenceClosure {

}
object CongruenceClosure extends CongruenceClosure {
  val congruence = closure _
  
  def closure(equalities: List[Equality[_]]): List[Equality[_]] = {
	val pending = equalities
	val useList = new ListMultiMap[Curried, Equality[_]]
	equalities foreach { case e @ Equality(Apply(a: Curried, b: Curried), c: Curried) => 
	  useList.put(a, e)
	  useList.put(b, e)
	}
	equalities
  }
  
  class ListMultiMap[A, B] {
	val map = new HashMap[A, scala.collection.mutable.ListBuffer[B]]
	def put(a: A, b: B) {
	  var list = map(a)
	  if (list == null)
	 	 list = new scala.collection.mutable.ListBuffer[B]
	  list.append(b)
	  map.put(a, list)
	}
  }
}