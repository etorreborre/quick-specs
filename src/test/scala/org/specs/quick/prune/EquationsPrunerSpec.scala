package org.specs.quick.prune
import org.specs.quick.equality._
import org.specs.quick.expression._
import org.specs._
import org.specs.quick.methods._
import org.specs.quick.expression._

class EquationsPrunerSpec extends SpecificationWithJUnit with EquationsPruner with SampleExpressions {
  "pruning equations must remove b = a if a = b is already there" in {
	prune(List(Equality(a, b), Equality(b, a))).mkString must_== "[a == b]" 
  }
  "pruning equations must not remove c = d if a = b is already there" in {
	prune(List(Equality(a, b), Equality(c, d))).mkString must_== "[c == d][a == b]" 
  }
  "pruning equations must not remove d + e = f if a + b = c is already there" in {
	prune(List(Equality(apply("+", a, b), c), Equality(apply("+", d, e), f))).mkString must_== "[+(d, e) == f][+(a, b) == c]" 
  }
  "pruning equations a + b = c, c = a + b" in {
	prune(List(Equality(apply("+", xs, nil), xs), Equality(xs, apply("+", xs, nil)))).mkString must_== "[+(xs, nil) == xs]" 
  }
  "pruning equations where there is a substitution" in {
	prune(List(Equality(apply("+", nil, nil), nil), Equality(apply("+", xs, nil), xs))).mkString must_== "[+(xs, nil) == xs]" 
  }
  "pruning equations where there is a substitution of a variable" in {
	prune(List(Equality(apply("+", xs, nil), xs), Equality(apply("+", ys, nil), ys))).mkString must_== "[+(xs, nil) == xs]" 
  }
  "not pruning equations where there is not a substitution of a variable" in {
	prune(List(Equality(apply("+", xs, nil), xs), Equality(apply("+", ints, nilInts), ints))).mkString must_== 
		"[+(xs, nil) == xs][+(ints, nilInts) == ints]" 
  }
  "the list of all bindings for an expression must be the cartesian product" +
  " of all possible values, using the types in a list of terms" in {
    val expression = ValuedExpressionWithVariables(xs)
	val terms = List(xs, ys)
    allBindings(expression, terms) must_== List(Bindings(expression, Map(xs -> xs)), Bindings(expression, Map(xs -> ys)))
  }
  "the possible values for an expression, using a list of terms is the list of a terms having the same type of each variable" in {
    val expression = ValuedExpressionWithVariables(xs, i)
    val terms = List(xs, ys, b)
	possibleValues(expression, terms) must_== List(List(xs, ys), List())
  }
  "the possible values for an expression, using a list of terms is the list of a terms having the same type of each variable - 2" in {
    val expression = ValuedExpressionWithVariables(xs)
    val terms = List(xs, nil)
	possibleValues(expression, terms) must_== List(List(xs, nil))
  }
}