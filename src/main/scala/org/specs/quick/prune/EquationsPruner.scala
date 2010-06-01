package org.specs.quick.prune
import org.specs.quick.expression._
import org.specs.quick.equality._
import EqualityFlattener._
import ExpressionCurrier._
import org.specs.quick.Functions._

trait EquationsPruner {
  val prune = pruneEquations _
  
  private def pruneEquations(equalities: List[Equality[Expression]]): List[Equality[_]] = {
	equalities |> curryfy |> flatten
  }
}