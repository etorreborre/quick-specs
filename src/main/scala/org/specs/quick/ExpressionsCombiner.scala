package org.specs.quick

trait ExpressionsCombiner extends Expressions {
  def combine(methods: List[ScalaMethod], variables: List[Variable[_]]): List[Expression] = {
    combine(methods.map(MethodExpression(_)) ::: variables.map(VariableExpression(_)))
  }
  def combine(expressions: List[Expression]): List[Expression] = {
    expressions.flatMap(_.apply(expressions))
  }
}