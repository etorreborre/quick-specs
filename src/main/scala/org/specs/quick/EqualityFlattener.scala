package org.specs.quick

trait EqualityFlattener { this: CurriedExpressions =>
  def flatten(curried: List[Equality]) = curried
}
