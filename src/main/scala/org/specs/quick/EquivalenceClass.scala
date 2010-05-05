package org.specs.quick

case class EquivalenceClass(expressions: List[Expression], variables: List[Variable[_]], result: Option[Any]) {
  def show = expressions.map(_.show).mkString(" == ")

  def this(expressions: List[Expression], variables: List[Variable[_]]) = this(expressions, variables, None)
  
  def equations = "Equivalence: " + result.getOrElse("None") + " -> " + show
  def equalities: List[ExpressionEquality] = makeEqualities(expressions)
  private def makeEqualities(exp: List[Expression]): List[ExpressionEquality] = exp match {
	case Nil => Nil
	case e :: Nil => List(new ExpressionEquality(e))
	case e :: other :: Nil => List(ExpressionEquality(e, other))
	case e :: other :: others => ExpressionEquality(e, other) :: makeEqualities(others)
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
trait Equality {
  def curryfy: Equality
}
case class ExpressionEquality(a: Expression, b: Expression) extends ExpressionCurrier with Equality {
  def this(a: Expression) = this(a, a)
  def curryfy: Equality = CurriedEquality(a.curryfy, b.curryfy)
}