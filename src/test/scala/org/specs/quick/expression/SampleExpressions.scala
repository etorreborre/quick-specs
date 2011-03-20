package org.specs.quick.expression
import org.specs.quick.equality._
import org.specs.quick.methods._
import org.specs.quick.expression._

trait SampleExpressions {
  implicit def exp(name: String): ConstantExpression = ConstantExpression(name)
  case class ConstantExpression(name: String) extends Expression with ValuedExpression {
    def getType = name
    def value = name
    override def hashCode = name.hashCode
    override def equals(o: Any) = name.equals(o)
    override def substitute(bindings: Map[Expression, ValuedExpression]) = this
    def variables: List[VariableExpression[_]] = Nil
  }
    
  def apply(m: String, o: ValuedExpression*) = {
	ApplicationExpression(FunctionExpression(method(m)), o.toList)
  }
  case class ValuedExpressionWithVariables(v: VariableExpression[_]*) extends ValuedExpression {
	def value = ""
	def getType = "String"
	def variables: List[VariableExpression[_]] = v.toList
	def substitute(bindings: Map[Expression, ValuedExpression]) = null
  }
  case class NoVariableExpression[A](constant: Constant[A]) extends ValuedExpression {
    lazy val getType = constant.getType
    def evaluate = constant.evaluate
    def value = constant.value
    override def show = constant.show
    def substitute(bindings: Map[Expression, ValuedExpression]) = this
    def variables: List[VariableExpression[_]] = Nil
  }
  val a: ConstantExpression = "a"
  val b: ConstantExpression = "b"
  val c: ConstantExpression = "c"
  val d: ConstantExpression = "d"
  val e: ConstantExpression = "e"
  val f: ConstantExpression = "f"
  val nil = new NoVariableExpression(new Constant("nil", Nil:List[String]))
  val nilInts: VariableExpression[List[Int]] = new VariableExpression(new Constant("nilInts", List(1)))
  val xs: VariableExpression[List[String]] = new VariableExpression(new Constant("xs", List("xs")))
  val ys: VariableExpression[List[String]] = new VariableExpression(new Constant("ys", List("ys")))
  val ints: VariableExpression[List[Int]] = new VariableExpression(new Constant("ints", List(1)))
  val i: VariableExpression[Int] = new VariableExpression(new Constant("i", 1))
  
  implicit def variableExpression[T](v: Variable[T]): VariableExpression[T] = new VariableExpression(v)
  implicit def functionIdExpression(m: String): FunctionExpression = FunctionExpression(method(m))
  def method(m: String) = new ScalaMethod {
	val method = getClass.getMethods().apply(0)
	def apply(values: Any*) = this
    override lazy val declaringClass: Class[_] = getClass
    override lazy val returnType: String = ""
    override lazy val parameterTypes: List[String] = List("")
    override lazy val methodName = m
  }
}