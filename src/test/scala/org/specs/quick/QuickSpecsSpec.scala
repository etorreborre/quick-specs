package org.specs.quick
import org.specs._
import org.scalacheck._

class QuickSpecsSpec extends SpecificationWithJUnit with QuickSpecs with Sugar {
  object Lists {
    def ++[T](l: List[T], l2: List[T]) = l ::: l2
    def nil[T]: List[T] = Nil
  }
  val plusPlus: ScalaMethod = Lists.methodByName("++").get
  implicit val params = Gen.defaultParams
  val xs = new Variable[List[Int]]("xs")
  val ys = new Variable[List[Int]]("ys")
  "creating quick specs" should {
    "return a list of equations for the most simple case" in {
      quick(Lists.accept("\\+\\+", "nil"), xs) must_==("++")
    }
    "return a list of equations containing the method name" in {
      quick(Lists, xs) must include("++")
    }
    "include only some methods" in {
      quick(Lists.accept("\\+\\+"), xs) must include("++") and not include("toString")
      quick(Lists.accept("toString"), xs) must include("toString") and not include("++")
    }
  }
  "the combine method" should {
    "combine an expression taking a 2 variables and 1 variable in 2 expressions: a, exp(a, a)" in {
      combine(List(plusPlus), List(xs)).toString must_== "List(++(xs, xs), xs)"
    }
    "combine an expression taking a 2 variables and 1 variables in 6 expressions: \n" + 
    "a, b, exp(a, b), exp(b, a), exp(a, a), exp(b, a)" in {
      combine(List(plusPlus), List(xs, ys)) must have size(6)
    }
  }
  "applicable parameters for a given method" should {
    "extract the same parameter types" in {
      val exp1 = new Expression { 
        def getType = "scala.List"
        override def toString = getType
        def evaluate = Nil
      }
      MethodExpression(plusPlus).applicableParameters(List(exp1)) must_== List(List(exp1, exp1))
    }
  }
  "applying expressions to a given expression" should {
    "return a list of expressions" in {
      val exp1 = new Expression { 
        def getType = "scala.List"
        override def toString = getType
        def evaluate = Nil
      }
      val plusPlusExpression = MethodExpression(plusPlus)
      plusPlusExpression.apply(List(exp1)) must_== List(ComposedExpression(plusPlusExpression, List(exp1, exp1)))
    }
  }
  "Evaluating expressions" should {
    "give a random value to a variable" in {
      val v1 = new Variable[Int]("v1")
      (1 to 10).toList.map(i => v1.evaluate).pp.exists(_ < 0) must beTrue 
    }
  }
}