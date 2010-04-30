package org.specs.quick
import org.specs.ScalaCheck
import org.specs._
import org.scalacheck._

trait QuickSpecs extends ScalaMethodsFactory with ExpressionsCombiner with ExpressionsClassifier with EquationsPruner { this: Specification =>

  def quick(a: AnyRef, variable: Variable[_]): String = quick(ScalaMethods.create(a), variable)
  def quick(methods: ScalaMethods, variable: Variable[_]): String = quick(methods, List(variable))
  def quick(methods: ScalaMethods, variables: List[Variable[_]]): String = {
    val combinations: List[Expression] = combine(methods.get, variables)
    val equivalenceClasses: List[EquivalenceClass] = classify(combinations)
    val pruned: List[EquivalenceClass] = prune(equivalenceClasses)
    pruned.map(_.equations) mkString "\n"
  } 
}

