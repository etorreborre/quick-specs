package org.specs.quick

trait EquationsPruner extends ExpressionCurrier with EqualityFlattener {
  def prune(classes: List[EquivalenceClass]) = {
	val curried: List[Equality[_]] = curryfy(classes)
	val flattened: List[Equality[_]] = flatten(curried)
	classes
  }
}