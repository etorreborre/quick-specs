package org.specs.quick.prune
import org.specs.quick.classify._
import org.specs.quick.equality._

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
  val flatten: Pair[List[Equality[_]], List[Equality[_]]] => (List[Equality[_]], List[Equality[_]]) = 
	  { p => flattenEqualitiesList(p._1, p._2) }
  
  def flattenEqualitiesList(original: List[Equality[_]], curried: List[Equality[_]]): (List[Equality[_]], List[Equality[_]]) = {
	val flattened = curried.foldLeft(Nil:List[Equality[_]]) { (res: List[Equality[_]], cur: Equality[_]) =>
	  flattenCurried(cur) ::: res
	}
	(original, flattened)
  }
  def flattenEqualities(curried: Equality[_]*): List[Equality[_]] = flattenEqualitiesList(Nil, curried.toList)._2
  def flattenCurried(curried: Equality[_]): List[Equality[_]] = {
	curried match {
	  case Equality(Curry(a), Curry(b)) => 
	    List(Equality(Curry(a), Curry(b)))
	  case Equality(Apply(Apply(a, b), c), d) => {
	    val ab = a.toString + b.toString
	    Equality(Apply(ab, c), d) :: flattenCurried(Equality(Apply(a, b), Curry(ab)))
	  }
	  case Equality(Apply(a, Curry(b)), c) => List(Equality(Apply(a, Curry(b)), c))
	  case Equality(a, b) if (a == b) => Nil
	  case Equality(a, b) if (a != b) => flattenCurried(Equality(b, a))
    }
  }
}
private[prune] object EqualityFlattener extends EqualityFlattener 


