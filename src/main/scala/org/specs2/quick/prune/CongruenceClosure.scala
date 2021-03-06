package org.specs2
package quick
package prune
import equality._
import scala.collection.mutable._
import org.specs2.collection.ListMultiMap

trait CongruenceClosure {
  val congruence = { p: Pair[List[Equality[_]], List[Equality[_]]] => closure(p._1, p._2) }
  
  private[prune] def closure(original: List[Equality[_]], equalities: List[Equality[_]]): List[Equality[_]] = {
    initialize(equalities)
    createClasses(equalities)
	equalities
  }
  private val useList = new ListMultiMap[Curried, Equality[_]]
  private val representative = new HashMap[Curried, Curried]
  private val classList = new ListMultiMap[Curried, Curried]
  private val lookup = new HashMap[(Curried, Curried), Curried]
  private val pending = new Stack[(Curried, Curried)]
  
  private def initialize(equalities: List[Equality[_]]) {
	equalities foreach {
	  case e @ Equality(Apply(a: Curried, b: Curried), c: Curried) => 
	    useList.put(a, e)
	    useList.put(b, e)
	    lookup += (a, b) -> c
	  case Equality(a: Curried, b: Curried) =>
	    pending.push((a, b))
	    List(a, b) foreach { v => 
	      representative += v -> v
	      classList.put(v, v)
	    }
	}

  }
  private def createClasses(equalities: List[Equality[_]]) {
    while (!pending.isEmpty) { 
      val (a, b) = pending.pop
      val (ra, rb) = (representative(a), representative(b)) 
      if (ra != rb && classList(ra).size <= classList(rb).size) {
        for (c <- classList(ra)) {
          representative(c) = rb
          classList.put(rb, c)
        }
        for (equality @ 
        	 Equality(Apply(c: Curried, d: Curried), e: Curried) <- useList(ra)) {
          val (rc, rd, re) = (representative(c), representative(d), representative(e)) 
          lookup.get(rc, rd) map { f => val rf = representative(f)
            if (rf != re) pending.push((re, rf))
          }	
          lookup((rc, rd)) = re
          useList.put(rb, equality)
        }
      } 
    }	  
  }
}
object CongruenceClosure extends CongruenceClosure