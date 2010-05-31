package org.specs.quick.classify
import org.specs.quick.expression._
import org.specs.quick.methods._

case class EquivalenceClass(expressions: List[Expression], variables: List[Variable[_]], result: Option[Any]) {
  def show = expressions.map(_.show).mkString(" == ")

  def this(expressions: List[Expression], variables: List[Variable[_]]) = this(expressions, variables, None)
  
  def equations = "Equivalence: " + result.getOrElse("None") + " -> " + show
  def equalities: List[ExpressionEquality] = makeEqualities(expressions)
  private def makeEqualities(exp: List[Expression]): List[ExpressionEquality] = exp match {
	case Nil => Nil
	case e :: Nil => List(new ExpressionEquality(e))
	case e :: other :: Nil if (e != other) => List(ExpressionEquality(e, other))
	case e :: other :: others if (e != other) => ExpressionEquality(e, other) :: makeEqualities(others)
	case _ => Nil
  }
  def evaluate: scala.collection.Map[Any, List[Expression]] = {
	variables.foreach(_.evaluate)
	expressions.groupBy(_.value)
  }
   
  private def partition: List[EquivalenceClass] =  evaluate.map { e =>  new EquivalenceClass(e._2, variables, Some(e._1)) }.toList
  def partition(n: Int): List[EquivalenceClass] = { 
    (1 to n).foldLeft(List(this)) { (res, cur) => res.flatMap(_.partition) }
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