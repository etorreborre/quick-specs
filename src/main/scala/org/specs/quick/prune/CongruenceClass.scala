package org.specs.quick.prune
import org.specs.quick.equality._
import org.specs.quick.expression._
import org.specs.collection.ListMultiMap
import scala.collection.mutable._
import org.specs.log._
abstract class CongruenceClass extends EqualityFlattener with Log {
  private val useList = new ListMultiMap[Curried, Equality[_]]
  private[prune] val representative = new HashMap[Curried, Curried]
  private val classList = new ListMultiMap[Curried, Curried]
  private val lookup = new HashMap[(Curried, Curried), Curried]
  private val pending = new Stack[(Curried, Curried)]
  def add[T](equality: T)(implicit conversion: T => Equality[Curried]) { 
	add(conversion(equality)) 
  }
  def add(equality: Equality[Curried]) {
	if (equality.isTautology) {
  	  info("    not adding the tautology"+equality)
	} else {
      info("    adding "+equality)
      initialize(equality)
	  recomputeCongruence()
	  debug("after adding\n"+toString)
	  assert(isCongruent(equality))
	}
  }
  def isCongruent[T](equality: T)(implicit conversion: T => Equality[Curried]): Boolean = isCongruent(conversion(equality)) 
  def isCongruent(equality: Equality[Curried]): Boolean = {
	val flattened:List[Equality[Curried]] = flattenCurried(equality)
    val congruent = flattened.forall { 
	  case e @ Equality(c @ Curry(_), Apply(a, b)) => 
	    debug("lookup for "+e+" "+(representative.get(c), lookup.get((a, b)).map(representative(_))))
	    representative.get(c).isDefined && 
	    lookup.get((a, b)).map(representative(_)) == representative.get(c)
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
  
  private def initialize(equality: Equality[Curried]) {
	debug("initializing with " + flattenCurried(equality).mkString("\n"))
	flattenCurried(equality) foreach  {
	  case e @ Equality(c: Curried, app @ Apply(a: Curried, b: Curried)) => 
	    useList.put(a, e)
	    useList.put(b, e)
	    register(a, b, c)
	    lookup += (a, b) -> c
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
	  if (!representative.get(v).isDefined) representative += v -> v
	  if (!classList(v).contains(v)) classList.put(v, v)
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
	debug("    classList "+ra+" "+classList(ra))
	debug("    classList "+rb+" "+classList(rb))
	if (classList(ra).size <= classList(rb).size) {
	  for (c <- classList(ra)) {
	    representative(c) = rb
	    if (!classList.apply(rb).contains(c)) classList.put(rb, c)
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
	  for (equality @ 
	       Equality(e: Curried, Apply(c: Curried, d: Curried)) <- useList(ra)) {
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
         "    lookup " + lookup).mkString("\n")

  }
}
object CongruenceClass extends ExpressionCurrier {
  implicit def curryfyExpressions(equality: Equality[Expression]): Equality[Curried] = equality.map(_.curryfy)
}
