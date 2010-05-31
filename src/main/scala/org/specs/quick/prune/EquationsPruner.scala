package org.specs.quick.prune
import org.specs.quick.classify._

trait EquationsPruner extends ExpressionCurrier with EqualityFlattener {
  val prune = pruneEquations _
  def pruneEquations(classes: List[EquivalenceClass]): List[Equality[_]] = {
	val curried: List[Equality[_]] = curryfy(classes)
	val flattened: List[Equality[_]] = flatten(curried)
	flattened
  }
}