package org.specs.quick
import org.specs.ScalaCheck
import org.specs.specification.Tagged
import java.lang.reflect.Method

trait QuickSpecs extends ScalaCheck { this: Specification =>

  def quick(a: AnyRef, variable: Variable[_]): String = quick(Methods.create(a), variable)
  def quick(methods: Methods, variable: Variable[_]): String = quick(methods, List(variable))
  def quick(methods: Methods, variables: List[Variable[_]]): String = {
    val combinations: List[Expression] = combine(methods.get, variables)
    val equivalenceClasses: List[EquivalenceClass] = classify(combinations)
    val pruned: List[EquivalenceClass] = prune(equivalenceClasses)
    pruned.map(_.equations) mkString "\n"
  } 

  def combine(methods: List[ScalaMethod], variables: List[Variable[_]]): List[Expression] = {
    combine(methods.map(MethodExpression(_)) ::: variables.map(VariableExpression(_)))
  }
  def combine(expressions: List[Expression]): List[Expression] = {
    expressions.flatMap(_.apply(expressions))
  }
  def classify(expressions: List[Expression]) = List(EquivalenceClass(0, expressions))
  def prune(classes: List[EquivalenceClass]) = classes
  trait Expression {
    def getType: String
    def apply(expressions: List[Expression]): List[Expression] = List(this)
  }
  case class ComposedExpression(main: Expression, other: List[Expression]) extends Expression {
    def getType = main.getType
    override def toString = main.toString + other.mkString("(", ", ", ")")
  }
  case class MethodExpression(m: ScalaMethod) extends Expression {
    override def toString = m.methodName
    def getType = m.getType
    def methodName = m.toString.replace("$plus", "+")
    def getVariableTypes: List[String] = m.getParameterTypes
    override def apply(expressions: List[Expression]) = {
      applicableParameters(expressions.filter(_ != this)) match {
        case Nil => Nil
        case combinations => combinations.map(combination => ComposedExpression(this, combination))
      }
    }
    def applicableParameters(expressions: List[Expression]): List[List[Expression]] = {
      cartesianProduct(getVariableTypes.map(t => expressions.filter(e => e.getType == t)))
    }
    def cartesianProduct(list: List[List[Expression]]): List[List[Expression]] = {
      list match {
        case Nil => List(List())
        case xs :: xss => for (y <- xs; ys <- cartesianProduct(xss)) yield y :: ys
      }
    }
  }
  case class VariableExpression(variable: Variable[_]) extends Expression {
    def getType = variable.getType
    override def toString = variable.toString
  }
  case class EquivalenceClass(value: Any, expressions: List[Expression]) {
    def equations = expressions.map(_.toString).mkString(" == ")
  }
  class Variable[A](name: String)(implicit m: scala.reflect.Manifest[A]) {
    def getType = m.erasure.getName
    override def toString = name
  }
  implicit def toMethods(a: AnyRef) = Methods.create(a)
  case class Methods(methods: List[ScalaMethod]) extends Tagged {
    def get: List[ScalaMethod] = methods.filter(_.isAccepted)
    override def taggedComponents = methods 
    def methodByName(name: String) = get.find(_.methodName == name)
  }
  case object Methods {
    def create(a: AnyRef) = Methods(a.getClass.getMethods.toList.map(ScalaMethod(_)))
  }
  case class ScalaMethod(m: Method) extends Tagged {
    def getType = m.getReturnType.getName
    override def toString = methodName
    def getParameterTypes = m.getParameterTypes.toList.map(_.getName)
    tag(methodName)
    def methodName = m.getName.replace("$plus", "+")
  }
}

class QuickSpecsSpec extends SpecificationWithJUnit with QuickSpecs with Sugar {
  object Lists {
    def ++[T](l: List[T], l2: List[T]) = l ::: l2
    def nil[T]: List[T] = Nil
  }
  val plusPlus: ScalaMethod = Methods.create(Lists).methodByName("++").get
  val list1 = new Variable[List[Int]]("list1")
  val list2 = new Variable[List[Int]]("list2")
  "creating quick specs" should {
    "return a list of equations containing the method name" in {
      quick(Lists, list1) must include("++")
    }
    "include only some methods" in {
      quick(Lists.accept("\\+\\+"), list1) must include("++") and not include("toString")
      quick(Lists.accept("toString"), list1) must include("toString") and not include("++")
    }
  }
  "the combine method" should {
    "combine an expression taking a 2 variables and 1 variable in 2 expressions: a, exp(a, a)" in {
      combine(List(plusPlus), List(list1)) must have size(2)
    }
    "combine an expression taking a 2 variables and 1 variables in 6 expressions: \n" + 
    "a, b, exp(a, b), exp(b, a), exp(a, a), exp(b, a)" in {
      combine(List(plusPlus), List(list1, list2)) must have size(6)
    }
  }
  "applicable parameters for a given method" should {
    "extract the same parameter types" in {
      val exp1 = new Expression { 
        def getType = "scala.List"
        override def toString = getType
      }
      MethodExpression(plusPlus).applicableParameters(List(exp1)) must_== List(List(exp1, exp1))
    }
  }
  "applying expressions to a given expression" should {
    "return a list of expressions" in {
      val exp1 = new Expression { 
        def getType = "scala.List"
        override def toString = getType
      }
      val plusPlusExpression = MethodExpression(plusPlus)
      plusPlusExpression.apply(List(exp1)) must_== List(ComposedExpression(plusPlusExpression, List(exp1, exp1)))
    }
  }
}