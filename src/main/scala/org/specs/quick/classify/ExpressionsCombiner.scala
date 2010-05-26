package org.specs.quick

trait ExpressionsCombiner extends Expressions {
  val combine = (combineMethods _).tupled
  private def combineMethods(methods: ScalaMethods, variables: List[Variable[_]]): EquivalenceClass = {
    combineMethodList(methods.get, variables)
  }
  private def combineMethodList(methods: List[ScalaMethod], variables: List[Variable[_]]): EquivalenceClass = {
    combineExpressions(makeExpressions(methods, variables), variables)
  }
  private def makeExpressions(methods: List[ScalaMethod], variables: List[Variable[_]]) = {
	methods.map(MethodExpression(_)) ::: variables.map(VariableExpression(_))
  }
  private def combineExpressions(expressions: List[Expression], variables: List[Variable[_]]): EquivalenceClass = {
    new EquivalenceClass(expressions.flatMap(_.apply(expressions)), variables)
  }
}
