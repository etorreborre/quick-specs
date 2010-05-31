package org.specs.quick.expression

trait Expression {
  def show: String = toString
  def getType: String
  def apply(expressions: List[Expression]): List[Expression] = List(this)
  def evaluate: Any
  def value: Any = evaluate
}

