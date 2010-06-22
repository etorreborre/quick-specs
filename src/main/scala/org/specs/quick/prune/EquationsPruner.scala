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
	println("the universe is " + universe)
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
  private def substitute(a: ValuedExpression, b: ValuedExpression, terms: List[ValuedExpression]) = {
	val bindings: List[Bindings] = allBindings(a, terms).distinct
	println("terms are " + terms)
	println("bindings are "+bindings)
	bindings.foldLeft(Nil:List[Equality[ValuedExpression]]) { (res, cur) => 
	  Equality(a.substitute(cur.toMap), b.substitute(cur.toMap)) :: res
	} 
  } 
  private def allBindings(a: ValuedExpression, terms: List[ValuedExpression]): List[Bindings] = {
	val possibleValues = a.variables.map { variable =>
	  terms.filter(t => t.getType == variable.getType) 
	}
	cartesianProduct(possibleValues).distinct.map { case params =>
	  val bindings = Bindings(a)
	  a.variables.zip(params) foreach { case (variable, value) =>
	    bindings.add(variable, value)
	  }
	  bindings
	}
	
  }
  case class Bindings(exp: Expression) {
	private val map = new scala.collection.mutable.HashMap[VariableExpression[_], ValuedExpression]
	def toMap: scala.collection.immutable.Map[Expression, ValuedExpression] = map.toMap
	def add(variable: VariableExpression[_], value: ValuedExpression) = map.put(variable, value); this
	override def toString = "\nexpression:\n" + exp.toString + "\n" + map.mkString("\n")
  }

}