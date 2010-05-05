package org.specs.quick

trait EquationsPruner extends ExpressionCurrier with EqualityFlattener {
  def prune(classes: List[EquivalenceClass]) = {
	val curried: List[Equality] = curryfy(classes)
	val flattened: List[Equality] = flatten(curried)
	classes
  }
}