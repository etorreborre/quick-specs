package org.specs2
package quick
package expression

import org.specs2.collection.CartesianProduct._
import methods._
import util.TypesMatcher

/**
 * Expression for a ScalaFunction
 * 
 * It can be applied a list of ValuedExpressions and result in a list of ApplicationExpressions where all possible
 * applications of some parameter combinations with the method are created.
 * 
 * It can be evaluated by passing actual values that should correspond to the parameters expected types (@see ScalaMethod) 
 */
case class FunctionExpression(function: ScalaFunction) extends ApplicableExpression with TypesMatcher {
  lazy val getType = function.returnType
  lazy val methodName = function.name
  override def show = function.name
  def show(parameters: Seq[String]) = function.show(parameters)
  def applyValues(values: Seq[Any]) = function.apply(values:_*)
  def apply(expressions: ValuedExpression*): Seq[ApplicationExpression] =  {
    if (expressions.isEmpty)
      Nil
    else
      applicableParameters(expressions:_*) map (params => ApplicationExpression(this, params))
  }

  /**
   * for each variable we keep a list of all the expressions that have the same type
   */
  private[expression] def applicableParameters(expressions: ValuedExpression*): Seq[Seq[ValuedExpression]] = {
	  val applicable: Seq[Seq[ValuedExpression]] = function.parameterTypes.map {	t =>
	    expressions.toList.filter(e => typesMatch(e.getType, t))
    }
	  val product = cartesianProduct(applicable.filter(_.nonEmpty))
    product.distinct
  }
}
