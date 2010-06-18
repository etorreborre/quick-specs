package org.specs.quick.prune
import org.specs.SpecificationWithJUnit
import org.specs.quick.equality._
import org.specs.quick.methods._
import org.specs.quick.expression._

class CongruenceClassSpec extends SpecificationWithJUnit("A congruence class for equalities") with ConstantExpressions with ExpressionCurrier {
  val congruence = new CongruenceClass
  "it" should {
	"accept simple equalities and check congruence for 2 constants" in {
1 must_== 1 
	}
	"accept equalities with an application and check congruence" in {
	  val eq1: Equality[Expression] = Equality(apply("+", xs, nil), xs)
	  val eq2: Equality[Expression] = Equality(xs, apply("+", xs, nil))
	  
	  congruence.add(eq1)

	  congruence.isCongruent(eq1) aka (eq1+" is already registered") must beTrue
	  congruence.isCongruent(eq2) aka (eq2+" is already registered") must beTrue
	}
  } 	
}
trait ConstantExpressions {
  implicit def exp(name: String): ConstantExpression = ConstantExpression(name)
  case class ConstantExpression(name: String) extends Expression with ValuedExpression {
    def getType = name
    def value = name
    override def hashCode = name.hashCode
    override def equals(o: Any) = name.equals(o)
  }
    
  def apply(m: String, o: ValuedExpression*) = {
	ApplicationExpression(MethodExpression(method(m)), o.toList)
  }

  val a: ConstantExpression = "a"
  val b: ConstantExpression = "b"
  val c: ConstantExpression = "c"
  val d: ConstantExpression = "d"
  val nil: ConstantExpression = "nil"
  val xs: ConstantExpression = "xs"
  val ys: ConstantExpression = "ys"
  implicit def methodExpression(m: String): MethodExpression = MethodExpression(method(m))
  def method(m: String) = new ScalaMethod {
	val method = getClass.getMethods().apply(0)
	def apply(values: List[Any]) = this
    override lazy val declaringClass: Class[_] = getClass
    override lazy val returnType: String = ""
    override lazy val parameterTypes: List[String] = List("")
    override lazy val methodName = m
  }
  

}