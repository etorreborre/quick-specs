package org.specs.quick.prune
import org.specs.quick.equality._
import org.specs.quick.expression._
import org.specs.collection.ListMultiMap
import scala.collection.mutable._

class CongruenceClass extends ExpressionCurrier with EqualityFlattener {
  private val useList = new ListMultiMap[Curried, Equality[_]]
  private val representative = new HashMap[Curried, Curried]
  private val classList = new ListMultiMap[Curried, Curried]
  private val lookup = new HashMap[(Curried, Curried), Curried]
  private val pending = new Stack[(Curried, Curried)]
  
  def add(equality: Equality[Expression]) {
	initialize(equality)
	recomputeCongruence()
  }
  def isCongruent(equality: Equality[Expression]): Boolean = {
	val Equality(a, b) = equality
	isCongruent(a, b)
  }
  def isCongruent(a: Expression, b: Expression): Boolean = {
	val Equality(u, v) = (flattenCurried(Equality(a.curryfy, b.curryfy)).collect { case e @ Equality(Curry(x), Curry(y)) => e }).apply(0)
	(representative.get(u), representative.get(v)) match {
	  case (Some(x), Some(y)) => x == y
	  case other => false
	}
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
      if (ra != rb && classList(ra).size <= classList(rb).size) {
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
    }	  
  }
  override def toString = {
	List("useList " + useList,
         "representative " + representative,
         "classList " + classList,
         "lookup " + lookup,
         "pending " +pending).mkString("\n")

  }
}