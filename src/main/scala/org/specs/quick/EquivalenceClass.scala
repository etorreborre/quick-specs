package org.specs.quick

case class EquivalenceClass(expressions: List[Expression], variables: List[Variable[_]], result: Option[Any]) {
  def show = expressions.map(_.show).mkString(" == ")

  def this(expressions: List[Expression], variables: List[Variable[_]]) = this(expressions, variables, None)
  
  def equations = "Equivalence: " + result.getOrElse("None") + " -> " + show
  
  def evaluate: scala.collection.Map[Any, List[Expression]] = {
	variables.foreach(_.evaluate)
	expressions.groupBy(_.value)
  }
   
  private def partition: List[EquivalenceClass] =  evaluate.map { e =>  new EquivalenceClass(e._2, variables, Some(e._1)) }.toList
  def partition(n: Int): List[EquivalenceClass] = { 
    (1 to n).foldLeft(List(this)) { (res, cur) => res.flatMap(_.partition) }
  }
}
