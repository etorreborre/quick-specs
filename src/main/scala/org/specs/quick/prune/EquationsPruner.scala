package org.specs.quick.prune
import org.specs.quick.classify._
import org.specs.quick.equality._

trait EquationsPruner extends ExpressionCurrier with EqualityFlattener {
  val prune = pruneEquations _
  
  private def pruneEquations(classes: List[EquivalenceClass]): List[Equality[_]] = {
	val curried: List[Equality[_]] = curryfy(classes.flatMap(_.equalities))
	val flattened: List[Equality[_]] = flatten(curried)
	flattened
  }
}