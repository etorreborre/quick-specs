package org.specs.quick.prune
import org.specs.quick.expression._
import org.specs.quick.equality._
import EqualityFlattener._
import ExpressionCurrier._
import CongruenceClosure._
import org.specs.quick.Functions._
import org.specs.quick.collection.CartesianProduct._

trait EquationsPruner extends org.specs.Sugar {
  val prune = pruneEquations _
  
  private def pruneEquations(equalities: List[Equality[ValuedExpression]]): List[Equality[_]] = {
	val congruence = new CongruenceClass
	val universe = equalities.foldLeft(Nil:List[ValuedExpression]) { (res, cur) => 
	  val Equality(a, b) = cur
	  a :: b ::: res
	}
	println("universe "+universe)
	equalities.sortBy(_.toString.size).foldLeft(Nil: List[Equality[_]]) { (res, cur) =>
	   if (congruence.isCongruent(cur))
	  	 res
	   else {
	  	congruence.add(cur)
    	val Equality(a: ValuedExpression, b: ValuedExpression) = cur

	  	substitute(a, b, universe).foreach { sub =>
	  	  val Equality(u, v) = sub
	  	  if (universe.contains(u) || universe.contains(v))
	  	 	congruence.add(sub)
	  	}
	  	cur :: res
	   }
	}.sortBy(_.toString.size)
  }
  
  private[prune] def substitute(a: ValuedExpression, b: ValuedExpression, terms: List[ValuedExpression]) = {
	val bindings: List[Bindings] = allBindings(a, terms).distinct
	bindings.foldLeft(Nil:List[Equality[ValuedExpression]]) { (res, cur) => 
	  Equality(a.substitute(cur.map), b.substitute(cur.map)) :: res
	} 
  } 
  
  private[prune] def allBindings(a: ValuedExpression, terms: List[ValuedExpression]): List[Bindings] = {
	cartesianProduct(possibleValues(a, terms)).map { case params =>
	  var bindings = Bindings(a, Map())
	  a.variables.zip(params) foreach { case (variable, value) =>
	    bindings = bindings.add(variable, value)
	  }
	  bindings
	}
  }
  
  private[prune] def possibleValues(a: ValuedExpression, terms: List[ValuedExpression]) = {
    a.variables.map { variable =>
      terms.filter(t => t.getType == variable.getType) 
    }
  }
  case class Bindings(exp: Expression, map: Map[Expression, ValuedExpression]) {
	def add(variable: VariableExpression[_], value: ValuedExpression) = Bindings(exp, map + (variable -> value))
	override def toString = "\nexpression:\n" + exp.toString + "\n" + map.mkString("\n")
  }
}