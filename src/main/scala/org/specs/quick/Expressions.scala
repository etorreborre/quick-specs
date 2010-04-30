package org.specs.quick

trait Expressions {
  implicit def toVariableExpression[A](v: Variable[A]) = VariableExpression(v)
}

