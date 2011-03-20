package org.specs.quick.prune
import org.specs.quick.expression._
import org.specs.util.TypesMatcher
import org.specs.quick.equality._
import EqualityFlattener._
import ExpressionCurrier._
import CongruenceClosure._
import org.specs.quick.Functions._
import org.specs.quick.collection.CartesianProduct._
import org.specs.log._
import org.specs.quick.prune.CongruenceClass._

trait EquationsPruner extends org.specs.Sugar with Log with TypesMatcher { outer =>
  val prune = pruneEquations _
  
  private def pruneEquations(equalities: List[Equality[ValuedExpression]]): List[Equality[_]] = { 
    val congruence = new CongruenceClass {
	  def println(m: Any) = outer.println(m)
	  def printf(format: String, args: Any*) = outer.printf(format, args)
	  level = outer.level
    }
	val universe = equalities.foldLeft(Nil:List[ValuedExpression]) { (res, cur) => 
	  val Equality(a, b) = cur
	  a :: b ::: res
	}
	debug("The equalities are "+equalities.mkString("\n"))
	debug("The universe is "+universe)
	val result = new scala.collection.mutable.ListBuffer[Equality[ValuedExpression]]
	equalities.sortBy(e => e)(equalityOrdering).foreach { cur =>
	  if (congruence.isCongruent(cur)) {
	 	debug(cur + " is congruent so is not added to the congruence structure")
	  }
	  else {
	 	debug(cur + " IS A NEW EQUATION!")
	  	congruence.add(cur)
	  	if (!cur.isTautology) result += cur

	  	val Equality(a: ValuedExpression, b: ValuedExpression) = cur
	 	debug("The subsitutes for "+cur+" are "+substitute(a, b, universe))
	  	substitute(a, b, universe).foreach { sub =>
	  	  val Equality(u, v) = sub
	  	  if (universe.contains(u) || universe.contains(v)) {
	  	 	congruence.add(sub)
	  	 	if (result.contains(sub)) {
    	 	  result -= sub
    	 	  debug("removing "+sub+" -> "+result)
    	 	}
          } else debug(u+" and "+v+" don't belong to the universe")
	  	}
	  }
	}
	result.toList.sortBy(_.toString.size)
  }
  
  private[prune] def substitute(a: ValuedExpression, b: ValuedExpression, terms: List[ValuedExpression]) = {
	val bindings: Seq[Bindings] = allBindings(a, terms).distinct
	bindings.foldLeft(Nil:List[Equality[ValuedExpression]]) { (res, cur) =>
	  val (subA, subB) = (a.substitute(cur.map), b.substitute(cur.map))
	  if ((a, b) != (subA, subB) && (a, b) != (subB, subA)) 
	    Equality(subA, subB) :: res
	  else 
	 	res
	}
  } 
  
  private[prune] def allBindings(a: ValuedExpression, terms: Seq[ValuedExpression]): Seq[Bindings] = {
	cartesianProduct(possibleValues(a, terms)).map { case params =>
	  var bindings = Bindings(a, Map())
	  a.variables.zip(params) foreach { case (variable, value) =>
	    bindings = bindings.add(variable, value)
	  }
	  bindings
	}
  }
  
  private[prune] def possibleValues(a: ValuedExpression, terms: Seq[ValuedExpression]) = {
    a.variables.map { variable =>
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
  object equalityOrdering extends Ordering[Equality[ValuedExpression]] {
	def variables(e: Equality[ValuedExpression]) = e match {
	  case Equality(a, b) => (a.variables ++ b.variables).sortBy(_.toString)
	}
	def compare(x: Equality[ValuedExpression], y: Equality[ValuedExpression]) = {
	  if (size(x) > size(y)) 1
	  else if (size(x) == size(y)) variables(x).toString.compareTo(variables(y).toString)
	  else -1
	}
	def size(x: Equality[ValuedExpression]): Int = {
	  val Equality(a, b) = x
	  size(a) + size(b)
	}
	def size(x: ValuedExpression): Int = {
	  x.variables.foldLeft(x.toString) { (res, cur) => 
	  	res.replace(cur.name, "v")
	  }.size
	}
  }

