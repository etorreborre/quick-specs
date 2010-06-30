package org.specs.quick.prune
import org.specs.quick.expression._
import org.specs.quick.equality._
import EqualityFlattener._
import ExpressionCurrier._
import CongruenceClosure._
import org.specs.quick.Functions._
import org.specs.quick.collection.CartesianProduct._
import org.specs.log._

trait EquationsPruner extends org.specs.Sugar with Log with TypesMatcher { outer =>
  val prune = pruneEquations _
  
  private def pruneEquations(equalities: List[Equality[ValuedExpression]]): List[Equality[_]] = { 
    val congruence = new CongruenceClass {
	  def println(m: Any) = outer.println(m)
	  def printf(format: String, args: Any*) = outer.printf(format, args)
	//  level = outer.level
    }
	val universe = equalities.foldLeft(Nil:List[ValuedExpression]) { (res, cur) => 
	  val Equality(a, b) = cur
	  a :: b ::: res
	}
	debug("The equalities are "+equalities.mkString("\n"))
	debug("The universe is "+universe)
	val result = new scala.collection.mutable.ListBuffer[Equality[ValuedExpression]]
	equalities.sortBy(_.toString.size).foreach { cur =>
	  if (congruence.isCongruent(cur)) {
	 	debug(cur + " is congruent so is not added to the congruence structure")
	  }
	  else {
	 	debug(cur + " IS A NEW EQUATION!")
	  	congruence.add(cur)
	  	result += cur
    	val Equality(a: ValuedExpression, b: ValuedExpression) = cur
	 	debug("The subsitutes for "+cur+" are "+substitute(a, b, universe))
	  	substitute(a, b, universe).foreach { sub =>
	  	  val Equality(u, v) = sub
	  	  if (universe.contains(u) || universe.contains(v)) {
	  	 	if (congruence.isCongruent(sub))
   	 	      debug(sub+" is already congruent")
   	 	    else {
   	 	      debug("adding the substitute "+sub+" to the congruence relationship")
	  	 	  congruence.add(sub)
	  	 	}
	  	 	if (variablesNumber(sub) > variablesNumber(cur)) {
  	 	      result -= cur
  	 	 	  result += sub
  	 	 	  debug("removing "+cur+" and adding "+"sub = "+result)
    	 	} else if (variablesNumber(sub) < variablesNumber(cur) && result.contains(sub)) {
    	 	  result -= sub
    	 	  debug("removing sub = "+result)
    	 	}
	  	  } else debug(u+" and "+v+" don't belong to the universe")
	  	}
	  }
	}
	result.toList.sortBy(_.toString.size)
  }
  
  private[prune] def substitute(a: ValuedExpression, b: ValuedExpression, terms: List[ValuedExpression]) = {
	val bindings: List[Bindings] = allBindings(a, terms).distinct
	bindings.foldLeft(Nil:List[Equality[ValuedExpression]]) { (res, cur) =>
	  val (subA, subB) = (a.substitute(cur.map), b.substitute(cur.map))
	  if (a != subA || b != subB)
	    Equality(subA, subB) :: res
	  else 
	 	res
		  
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
      debug("variables for expression a "+variable)
      terms.filter(t => typesMatch(t.getType, variable.getType)) 
    }
  }
  
  private def variablesNumber(equality: Equality[ValuedExpression]) = {
	val Equality(a, b) = equality
	(a.variables ::: b.variables).distinct.size
  }
  case class Bindings(exp: Expression, map: Map[Expression, ValuedExpression]) {
	def add(variable: VariableExpression[_], value: ValuedExpression) = Bindings(exp, map + (variable -> value))
	override def toString = "\nexpression:\n" + exp.toString + "\n" + map.mkString("\n")
  }
}