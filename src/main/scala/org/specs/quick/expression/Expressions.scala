package org.specs.quick.expression
import org.specs.quick.methods._

trait Expressions {
  implicit def toVariableExpression[A](v: Variable[A]) = VariableExpression(v)
}

