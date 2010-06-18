package org.specs.quick.prune
import org.specs.quick.expression._
import org.specs.quick.equality._
import EqualityFlattener._
import ExpressionCurrier._
import CongruenceClosure._
import org.specs.quick.Functions._

trait EquationsPruner extends org.specs.Sugar {
  val prune = pruneEquations _
  
  private def pruneEquations(equalities: List[Equality[Expression]]): List[Equality[_]] = {
	val congruence = new CongruenceClass
	equalities.foldLeft(Nil: List[Equality[_]]) { (res, cur) =>
	   if (congruence.isCongruent(cur))
	  	 res
	   else {
	  	congruence.add(cur)
	  	cur :: res
	   }
	}
  }
}