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
  "pruning equations a + b = c, c = a + b" in {
	prune(List(Equality(apply("+", xs, nil), xs), Equality(xs, apply("+", xs, nil)))).mkString must_== "[+(xs, nil) == xs]" 
  }
}