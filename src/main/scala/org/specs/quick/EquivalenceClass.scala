package org.specs.quick

case class EquivalenceClass(expressions: List[Expression]) {
  def equations = expressions.map(_.toString).mkString(" == ")
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
      evaluate.values.map(expressions => EquivalenceClass(expressions)).toList ::: res
    }  
  }
}
