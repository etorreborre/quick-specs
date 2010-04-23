package org.specs.quick

case class EquivalenceClass(expressions: List[Expression], result: Option[Any]) {
  def this(expressions: List[Expression]) = this(expressions, None)
  def equations = "Equivalence: " + result + " -> " +expressions.map(_.show).mkString(" == ")
  def evaluate: scala.collection.mutable.Map[Any, List[Expression]] = {
    expressions.foldLeft(new scala.collection.mutable.HashMap[Any, List[Expression]]) { (res, cur) =>
      val evaluated = cur.evaluate
      var expressionsWithSameResult = res.get(evaluated).getOrElse(List[Expression]())
      res.put(evaluated, cur :: expressionsWithSameResult)
      res
    }
  }
    
  def partition(n: Int) = {
    (1 to n).foldLeft(List[EquivalenceClass]()) { (res, cur) =>
      evaluate.map { e =>
        EquivalenceClass(e._2, Some(e._1)) 
      }.toList ::: res
    }
  }
}
