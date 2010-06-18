package org.specs.quick.prune
import org.specs.quick.classify._
import org.specs.quick.equality._
import org.specs.quick.expression._
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
  val flatten: Pair[List[Equality[Curried]], List[Equality[Curried]]] => (List[Equality[Curried]], List[Equality[Curried]]) = 
	  { p => flattenEqualitiesList(p._1, p._2) }
  
  def flattenEqualitiesList(original: List[Equality[Curried]], curried: List[Equality[Curried]]): (List[Equality[Curried]], List[Equality[Curried]]) = {
	val flattened = curried.foldLeft(Nil:List[Equality[Curried]]) { (res: List[Equality[Curried]], cur: Equality[Curried]) =>
	  flattenCurried(cur) ::: res
	}
	(original, flattened)
  }
  def flattenEqualities(curried: Equality[Curried]*): List[Equality[Curried]] = flattenEqualitiesList(Nil, curried.toList)._2
  def flattenCurried(curried: Equality[Curried]): List[Equality[Curried]] = {
	curried match {
   	  case Equality(Curry(a), Curry(b)) if (a == b) => List()
   	  case Equality(Curry(a), Curry(b)) if (a != b) => List(curried)

   	  case Equality(Apply(Curry(a), Curry(b)), Curry(c)) => List(curried)
	  
	  case Equality(Apply(a, b), c) => {
	    val flattenA = flattenCurried(Equality(a, Curry(newId(a))))
	    val flattenB = flattenCurried(Equality(b, Curry(newId(b))))
	    val flattenAB = flattenCurried(Equality(Apply(Curry(newId(a)), Curry(newId(b))), Curry(c)))
	    val flattenC = flattenCurried(Equality(c, Curry(newId(c))))
	    flattenA ::: flattenB ::: flattenAB ::: flattenC
	  }
	  case Equality(c, Apply(a, b)) => flattenCurried(Equality(Apply(a, b), Curry(c)))
	  
	  case _ => println("Not in a curried form " + curried.a.show + " and " + curried.b.show); Nil 
    }
  }
  def newId(a: Curried) = a.hashCode.toString
}
private[prune] object EqualityFlattener extends EqualityFlattener 


