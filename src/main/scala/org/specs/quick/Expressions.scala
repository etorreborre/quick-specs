package org.specs.quick

trait Expressions {
  implicit def toVariableExpression[A](v: Variable[A]) = VariableExpression(v)
}
trait Expression {
  def show: String = toString
  def getType: String
  def apply(expressions: List[Expression]): List[Expression] = List(this)
  def evaluate: Any
}
case class ComposedExpression(main: MethodExpression, other: List[Expression]) extends Expression {
  def getType = main.getType
  override def toString = main.toString + other.mkString("(", ", ", ")")
  def evaluate = {
    try {
      main.applyValues(other.map(_.evaluate))
    } catch {
      case e => e
    }
  }
}
case class MethodExpression(m: ScalaMethod) extends Expression {
  override def toString = m.methodName
  def getType = m.getType
  def methodName = m.toString.replace("$plus", "+")
  def getVariableTypes: List[String] = m.getParameterTypes
  def signature = methodName + getVariableTypes.mkString("(", ",", ")") + ": " + getType
  override def apply(expressions: List[Expression]) = {
    if (getVariableTypes.isEmpty) {
      List(ComposedExpression(this, Nil))
    } else {
      applicableParameters(expressions.filter(_ != this)) match {
        case Nil => Nil 
        case combinations => combinations.map(combination => ComposedExpression(this, combination))
      }
    }
  }
  def evaluate = applyValues(List())
  def applyValues(values: List[Any]) = {
    m.apply(values)
  }
  def applicableParameters(expressions: List[Expression]): List[List[Expression]] = {
    cartesianProduct(getVariableTypes.map(t => expressions.filter(e => e.getType == t)))
  }
  def cartesianProduct(list: List[List[Expression]]): List[List[Expression]] = {
    list match {
      case Nil => List(List())
      case xs :: xss => for (y <- xs; ys <- cartesianProduct(xss)) yield y :: ys
    }
  }
}
case class VariableExpression[A](variable: Variable[A]) extends Expression {
  def getType = variable.getType
  override def toString = variable.show
  def evaluate = variable.value
}
