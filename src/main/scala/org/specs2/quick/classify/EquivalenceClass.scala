package org.specs2.quick
package classify
import expression._
import equality._
import methods._

/**
 * An equivalence class is a list of Valued Expressions which give the same result for a given set 
 * of variables
 */
private[classify] case class EquivalenceClass(expressions: Seq[ValuedExpression], variables: Seq[Variable[_]], result: Option[Any]) {

  /** create an equivalence class with no result yet */
  def this(expressions: Seq[ValuedExpression], variables: Seq[Variable[_]]) = this(expressions, variables, None)
  /**
   *  recursively partition this equivalence class n times
   */
  def partition(n: Int): List[EquivalenceClass] = { 
    (List(this) /: (1 to n)) { (res, cur) => res.flatMap(_.partition) }
  }

  /**
   * @return a list of EquivalenceClass where each class groups expression having the same result
   */
  private def partition: List[EquivalenceClass] = evaluate.map { case (result, exps) =>  
    new EquivalenceClass(exps, variables, Some(result)) 
  }.toList
  /**
   * @return a map of expressions sorted by result
   */
  private def evaluate: scala.collection.Map[Any, Seq[ValuedExpression]] = {
	variables.foreach(_.evaluate)
	expressions.groupBy(_.value)
  }

  def equalities: Seq[Equality[ValuedExpression]] = {
	(variables.map(v => Equality(VariableExpression(v), VariableExpression(v))) ++ makeEqualities(expressions.toList)).distinct
  }
  private def makeEqualities(exp: List[ValuedExpression]): List[Equality[ValuedExpression]] = exp.sortBy(_.toString.size).toList match {
	case Nil => Nil
	case e :: Nil => List(Equality(e, e))
	case e :: other :: Nil if (e != other) => List(Equality(e, other))
	case e :: other :: others if (e != other) => Equality(e, other) :: makeEqualities(e :: others)
	case _ => Nil
  }
  override def toString = "Expressions: "+expressions+" Variables: "+variables+result.map(" Result: "+_).getOrElse("")
}
