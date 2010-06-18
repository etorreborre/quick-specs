package org.specs.quick.prune
import org.specs.quick.equality._
import org.specs.quick.expression._
import org.specs._
import org.specs.quick.methods._
import org.specs.quick.expression._

class EquationsPrunerSpec extends SpecificationWithJUnit with EquationsPruner with ConstantExpressions {
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
	prune(List(Equality(apply("+", xs, nil), xs), Equality(apply("+", nil, nil), nil))).mkString must_== "[+(xs, nil) == xs]" 
  }
}