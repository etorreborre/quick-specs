package org.specs.quick
import org.specs.ScalaCheck
import org.specs._
import org.scalacheck._

/**
 * This trait allows the creation of equations satisfied by some methods of a Scala object,
 * according to a list of variables.
 *
 * The output of the quick method is a list of equations, one equation per line
 */
trait QuickSpecs extends ScalaMethodsFactory with ExpressionsCombiner with ExpressionsClassifier with EquationsPruner { this: org.specs.Specification =>

  def quick(methods: ScalaMethods, variables: Variable[_]*): String = quick(methods, variables.toList)
  
  /**
   * 1. combine methods and variables to from expressions
   * 2. test them with ScalaCheck to sort them into equivalence classes
   * 3. prune redondant equations
   */
  private def quick(methods: ScalaMethods, variables: List[Variable[_]]): String = {
    (combine andThen classify andThen prune)(methods.get, variables) mkString "\n"
  } 
}

