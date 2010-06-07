package org.specs.quick.prune
import org.specs.quick.equality._
import org.specs.collection.ListMultiMap

trait CongruenceClosure {
  val congruence = closure _
  
  private[prune] def closure(equalities: List[Equality[_]]): List[Equality[_]] = {
	val pending = equalities
	val useList = new ListMultiMap[Curried, Equality[_]]
	equalities foreach { case e @ Equality(Apply(a: Curried, b: Curried), c: Curried) => 
	  useList.put(a, e)
	  useList.put(b, e)
	}
	equalities
  }

}
object CongruenceClosure extends CongruenceClosure