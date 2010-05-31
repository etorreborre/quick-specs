package org.specs.quick.classify
import org.specs.quick.expression._
import org.specs.quick.methods._

/**
 * An equivalence class is a list of Valued Expressions which give the same result for a given set 
 * of variables
 */
case class EquivalenceClass(expressions: List[ValuedExpression], variables: List[Variable[_]], result: Option[Any]) {

  /** create an equivalence class with no result yet */
  def this(expressions: List[ValuedExpression], variables: List[Variable[_]]) = this(expressions, variables, None)
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
  private def evaluate: scala.collection.Map[Any, List[ValuedExpression]] = {
	variables.foreach(_.evaluate)
	expressions.groupBy(_.value)
  }

  def show = expressions.map(_.show).mkString(" == ")
  def equations = "Equivalence: " + result.getOrElse("None") + " -> " + show
  
  def equalities: List[ExpressionEquality] = makeEqualities(expressions)
  private def makeEqualities(exp: List[Expression]): List[ExpressionEquality] = exp match {
	case Nil => Nil
	case e :: Nil => List(new ExpressionEquality(e))
	case e :: other :: Nil if (e != other) => List(ExpressionEquality(e, other))
	case e :: other :: others if (e != other) => ExpressionEquality(e, other) :: makeEqualities(others)
	case _ => Nil
  }
}
trait Curryfiable {
  def curryfy: this.type
}
case class ExpressionEquality(a: Expression, b: Expression) extends Equality[Expression](a, b) {
  def this(a: Expression) = this(a, a)
}
class Equality[T](a: T, b: T) {
  def this(a: T) = this(a, a)
  override def toString = "[" + a.toString + " == " + b.toString + "]" 
}