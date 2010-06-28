package org.specs.quick.prune
import org.specs.quick.equality._
import org.specs.quick.expression._
import org.specs.collection.ListMultiMap
import scala.collection.mutable._
import org.specs.log._

abstract class CongruenceClass extends ExpressionCurrier with EqualityFlattener with Log {
  private val useList = new ListMultiMap[Curried, Equality[_]]
  private val representative = new HashMap[Curried, Curried]
  private val classList = new ListMultiMap[Curried, Curried]
  private val lookup = new HashMap[(Curried, Curried), Curried]
  private val pending = new Stack[(Curried, Curried)]
  
  def add(equality: Equality[Expression]) {
	info("    adding "+equality)
	initialize(equality)
	recomputeCongruence()
	assert(isCongruent(equality))
  }
  def isCongruent(equality: Equality[Expression]): Boolean = {
	debug("    testing the congruence of "+equality)
	debug(toString)
	val flattened:List[Equality[Curried]] = flattenCurried(equality.map(_.curryfy))
    val congruent = flattened.forall { 
	  case e @ Equality(Apply(a, b), c @ Curry(_)) => 
	    debug("lookup for "+e+" "+(representative.get(c), lookup.get((a, b)).map(representative(_))))
	    representative.get(c).isDefined && 
	    lookup.get((a, b)).map(representative(_)) == representative.get(c)
	  case e @ Equality(a, b) => 
	    debug("rep for "+e+" "+(representative.get(a), representative.get(b)))
	    representative.get(a).isDefined && 
	    representative.get(a) == representative.get(b)
	}
	debug("    "+equality+" is congruent "+congruent)
	congruent
  }
  
  private def initialize(equality: Equality[Expression]) {
	flattenCurried(equality.map(_.curryfy)) foreach  {
		
	  case e @ Equality(app @ Apply(a: Curried, b: Curried), c: Curried) => 
	    useList.put(a, e)
	    useList.put(b, e)
	    register(a, b, c)
	    lookup += (a, b) -> c
	  case e @ Equality(a: Curried, b: Curried) =>
	    pending.push((a, b))
	    register(a, b)
	}
	def register(elements: Curried*) = elements foreach { v => 
	  representative += v -> v
	  classList.put(v, v)
	}
  }
  private def recomputeCongruence() {
    while (!pending.isEmpty) { 
      val (a, b) = pending.pop
      val (ra, rb) = (representative(a), representative(b))
      if (ra != rb) {
        update(ra, rb)
      } 
    }	  
  }
  private def update(ra: Curried, rb: Curried) {
	debug("    classList(ra) "+classList(ra))
	debug("    classList(rb) "+classList(rb))
	if (classList(ra).size <= classList(rb).size) {
	  for (c <- classList(ra)) {
	    representative(c) = rb
	    classList.put(rb, c)
	  }
	  for (equality @ 
	       Equality(Apply(c: Curried, d: Curried), e: Curried) <- useList(ra)) {
	    val (rc, rd, re) = (representative(c), representative(d), representative(e))
	    lookup.get(rc, rd) map { f => 
	      val rf = representative(f)
	      if (rf != re) pending.push((re, rf))
	    }	
	    lookup((rc, rd)) = re
	    useList.put(rb, equality)
	  }
    }
	else update(rb, ra)
		
  }
  override def toString = {
	List("    useList " + useList,
         "    representative " + representative,
         "    classList " + classList,
         "    lookup " + lookup,
         "    pending " +pending).mkString("\n")

  }
}