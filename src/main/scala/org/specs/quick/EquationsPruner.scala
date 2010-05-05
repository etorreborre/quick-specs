package org.specs.quick

trait EquationsPruner extends ExpressionCurrier {
  def prune(classes: List[EquivalenceClass]) = {
	val curried: List[Curried] = curryfy(classes)
	classes
  }
}