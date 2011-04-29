package org.specs2
package quick
package prune
import equality._
import expression._
import scala.collection.mutable._
import org.specs2.collection.ListMultiMap

abstract class CongruenceClass extends EqualityFlattener {

  private val useList = new ListMultiMap[Curried, Equality[_]]
  private[prune] val representative = new HashMap[Curried, Curried]
  private val classList = new ListMultiMap[Curried, Curried]
  private val lookup = new HashMap[(Curried, Curried), Curried]
  private val pending = new Stack[(Curried, Curried)]

  def add[T](equality: T)(implicit conversion: T => Equality[Curried], args: Args = Args()) {
	  addEquality(conversion(equality))
  }
  def addEquality(equality: Equality[Curried])(implicit args: Args = Args()) {
	  if (equality.isTautology) {
    	  if (args.verbose.congruent) println("    not adding the tautology"+equality)
	  } else {
        if (args.verbose.congruent) println("    adding "+equality)
      initialize(equality)
	    recomputeCongruence()
	    if (args.verbose.congruent) println("after adding\n"+toString)
	    assert(isCongruent(equality))
	  }
  }
  def isCongruent[T](equality: T)(implicit conversion: T => Equality[Curried], args: Args = Args()): Boolean = isCongruentEquality(conversion(equality))

  def isCongruentEquality(equality: Equality[Curried])(implicit args: Args = Args()): Boolean = {
	  val flattened:List[Equality[Curried]] = flattenCurried(equality)
    val congruent = flattened.forall { 
	  case e @ Equality(c @ Curry(_), Apply(a, b)) => 
	    if (args.verbose.congruent) println("lookup for "+e+" "+(representative.get(c), lookup.get((a, b)).map(representative(_))))
	    representative.get(c).isDefined && 
	    lookup.get((a, b)).map(representative(_)) == representative.get(c)
	  case e @ Equality(Apply(a, b), c @ Curry(_)) => 
	    if (args.verbose.congruent) println("lookup for "+e+" "+(representative.get(c), lookup.get((a, b)).map(representative(_))))
	    representative.get(c).isDefined && 
	    lookup.get((a, b)).map(representative(_)) == representative.get(c)
	  case e @ Equality(a, b) => 
	    if (args.verbose.congruent) println("rep for "+e+" "+(representative.get(a), representative.get(b)))
	    representative.get(a).isDefined && 
	    representative.get(a) == representative.get(b)
	  }
	  if (args.verbose.congruent) println("    "+equality+" is congruent "+congruent)
	  congruent
  }
  
  private def initialize(equality: Equality[Curried])(implicit args: Args = Args()) {
	  if (args.verbose.congruent) println("initializing with " + flattenCurried(equality).mkString("\n"))
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
  private def update(ra: Curried, rb: Curried)(implicit args: Args = Args()) {
	  if (args.verbose.congruent) println("    classList "+ra+" "+classList(ra))
	  if (args.verbose.congruent) println("    classList "+rb+" "+classList(rb))
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
