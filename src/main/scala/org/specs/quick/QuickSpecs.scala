package org.specs.quick
import org.specs.quick.methods._
import org.specs.quick.expression._
import org.specs.quick.classify._
import org.specs.quick.prune._
import org.specs._
import org.scalacheck._

/**
 * This trait allows the creation of equations satisfied by some methods of a Scala object,
 * according to a list of variables.
 * 
 * The ScalaMethods parameter contains a list of methods that need to be combined to generate the equations
 *
 * The output of the quick method is a list of equations, one equation per line
 */
trait QuickSpecs extends ScalaMethodsFactory with ExpressionsCombiner with ExpressionsClassifier with EquationsPruner with Functions with DefaultParams { this: org.specs.Specification =>
  /**
   * @return a list of equations for a given list of methods and variables
   */
  def quick(methods: ScalaMethods, variables: Variable[_]*): String = quick(methods, variables.toList)
  
  /**
   * 1. combine methods and variables to from expressions
   * 2. test them with ScalaCheck to sort them into equivalence classes
   * 3. prune redundant equations
   */
  private def quick(methods: ScalaMethods, variables: List[Variable[_]]): String = {
    (methods, variables) |> combine |> classify |> prune |>  (_ mkString "\n")
  } 
}
trait DefaultParams {
  implicit val params = Gen.Params(2, org.scalacheck.util.StdRand)	
}
