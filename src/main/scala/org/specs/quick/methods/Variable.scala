package org.specs.quick.methods
import org.scalacheck._
import scala.reflect.ClassManifest

/**
 * Trait representing a Variable of some type A
 * 
 * It has a name (usually given by the user) and a type (defined as a String coding for the full name of a class)
 * 
 * The evaluate method can be used to determine a new value for the variable. 'value' returns the result of the last evaluation
 */
trait Variable[A] {
  val name: String
  def getType: String
  def value: A
  def evaluate: A
  def show = name
  override def hashCode = getType.hashCode
  override def toString = show
}
/**
 * An arbitrary variable is a variable which change its value on each call to the evaluate method 
 */
class ArbitraryVariable[A](val name: String)(implicit m: ClassManifest[A], arb: Arbitrary[A], params: Gen.Params) extends Variable[A] {
  private var arbValue: A = evaluate
  
  def getType = m.toString
  def value: A = arbValue
  def evaluate = { arbValue = arb.arbitrary.apply(params).get; value }
}
/**
 * This variable always return the same value
 */
class Constant[A](val name: String, v: A)(implicit m: ClassManifest[A]) extends Variable[A] {
  def getType = m.toString
  def value = evaluate
  def evaluate = v
}
/**
 * factory method to create an Arbitrary variable that will change its value on each evaluate call
 */
object Variable {
  def apply[A](name: String)(implicit m: ClassManifest[A], arb: Arbitrary[A], params: Gen.Params) = new ArbitraryVariable(name)(m, arb, params)
}
object Constant {
  def apply[A](name: String)(v: A)(implicit m: ClassManifest[A]): Constant[A] = new Constant(name, v)(m)
}
