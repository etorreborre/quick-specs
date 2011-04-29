package org.specs2.quick
package prune
import classify._
import equality._
import expression._
/**
 * This trait "flattens" equalities by introducing new equalities so that, for example:
 * 
 * .(.(a, b), c) = d => 
 * 
 * .(ab, c) = d
 * .(a, b) = ab 
 * 
 * where ab is a fresh expression
 */
private[prune] trait EqualityFlattener { 
  def flatten(implicit args: Args = Args()): Pair[List[Equality[Curried]], List[Equality[Curried]]] => (List[Equality[Curried]], List[Equality[Curried]]) =
	  { p => flattenEqualitiesList(p._1, p._2) }
  
  def flattenEqualitiesList(original: List[Equality[Curried]], curried: List[Equality[Curried]])(implicit args: Args = Args()): (List[Equality[Curried]], List[Equality[Curried]]) = {
	  val flattened = curried.foldLeft(Nil:List[Equality[Curried]]) { (res: List[Equality[Curried]], cur: Equality[Curried]) =>
	    flattenCurried(cur) ::: res
	  }
	  (original, flattened.distinct)
  }

  def flattenEqualities(curried: Equality[Curried]*)(implicit args: Args = Args()): List[Equality[Curried]] = flattenEqualitiesList(Nil, curried.toList)._2

  def flattenCurried(curried: Equality[Curried])(implicit args: Args = Args()): List[Equality[Curried]] = {
	  curried match {
     	  case Equality(Curry(a), Curry(b)) if (a == b) => List()
     	  case Equality(Curry(a), Curry(b)) if (a != b) => List(curried)

     	  case Equality(ab @ Apply(a, b), Curry(c)) => {
	        val flattenA = flattenCurried(Equality(a, newCurry(a)))
	        val flattenB = flattenCurried(Equality(b, newCurry(b)))
	        val flattenAB = List(Equality(Apply(newCurry(a), newCurry(b)), newCurry(ab)))
	        val flattenABC = List(Equality(newCurry(ab), Curry(c)))
	        flattenABC ::: flattenAB  ::: flattenA ::: flattenB
     	  }
     	  case Equality(Curry(c), ab @ Apply(a, b)) => flattenCurried(Equality(ab, Curry(c)))

	    case Equality(ab @ Apply(a, b), cd @ Apply(c, d)) => {
	      val flattenA = flattenCurried(Equality(a, newCurry(a)))
	      val flattenB = flattenCurried(Equality(b, newCurry(b)))
	      val flattenC = flattenCurried(Equality(c, newCurry(c)))
	      val flattenD = flattenCurried(Equality(c, newCurry(c)))
	      val flattenAB = flattenCurried(Equality(newCurry(ab), Apply(newCurry(a), newCurry(b))))
	      val flattenCD = flattenCurried(Equality(newCurry(cd), Apply(newCurry(c), newCurry(d))))
	      val flattenABCD = flattenCurried(Equality(newCurry(ab), newCurry(cd)))
	      flattenABCD ::: flattenAB ::: flattenCD ::: flattenA  ::: flattenB ::: flattenC ::: flattenD
	    }
	    case _ => if (args.verbose.flatten) println("Not in a curried form " + curried.a.show + " and " + curried.b.show); Nil
    }
  }
  def newCurry(a: Curried): Curry = {
	  a match {
	    case Curry(u: Curried) => Curry(newId(u))
	    case Curry(other) => Curry(other)
	    case Apply(u, v) => Curry(newCurry(u).value+newCurry(v).value)
	    case other => Curry(newId(a))
	  }
  }
  def newId(a: Curried) = a.show+":"+a.hashCode.toString
}
private[prune] object EqualityFlattener extends EqualityFlattener 


