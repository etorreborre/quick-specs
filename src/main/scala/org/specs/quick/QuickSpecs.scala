package org.specs.quick
import org.specs.ScalaCheck
import org.specs._
import org.scalacheck._

trait QuickSpecs extends MethodsFactory with Expressions with ScalaCheck { this: Specification =>

  def quick(a: AnyRef, variable: Variable[_]): String = quick(ScalaMethods.create(a), variable)
  def quick(methods: ScalaMethods, variable: Variable[_]): String = quick(methods, List(variable))
  def quick(methods: ScalaMethods, variables: List[Variable[_]]): String = {
    val combinations: List[Expression] = combine(methods.get, variables)
    val equivalenceClasses: List[EquivalenceClass] = classify(combinations)
    val pruned: List[EquivalenceClass] = prune(equivalenceClasses)
    pruned.map(_.equations) mkString "\n"
  } 

  def combine(methods: List[ScalaMethod], variables: List[Variable[_]]): List[Expression] = {
    combine(methods.map(MethodExpression(_)) ::: variables.map(VariableExpression(_)))
  }
  def combine(expressions: List[Expression]): List[Expression] = {
    expressions.flatMap(_.apply(expressions))
  }
  def classify(expressions: List[Expression]) = {
    new EquivalenceClass(expressions).partition(4)
  }
  def prune(classes: List[EquivalenceClass]) = classes
}

