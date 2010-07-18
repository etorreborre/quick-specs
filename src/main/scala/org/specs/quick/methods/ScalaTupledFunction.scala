package org.specs.quick.methods
import Function._
import org.specs.quick.Functions._

class ScalaTupledFunction[T, R](aName: String, function: T => R)(implicit t: Manifest[T], r: Manifest[R])
  extends ScalaFunction {
  def this(function: T => R)(implicit t: Manifest[T], r: Manifest[R]) = this("", function)(t, r)
  def name: String = aName
  def returnType = r.toString
  def parameterTypes: Seq[String] = {
	if (t.typeArguments.isEmpty)
	  t.toString.split(",")
	else
	  t.typeArguments.map(_.toString)
  }
  def apply(values: Any*): Any = {
	try {
	  function.apply(toTuple(values).asInstanceOf[T])
	} catch {
	  case e: ClassCastException => throw new ScalaFunctionApplyException(e)
	}
  }
}
case class ScalaFunctionApplyException(e: Exception) extends Exception(e)

class ScalaZeroArgFunction[R](aName: String, function: () => R)(implicit r: Manifest[R])
  extends ScalaFunction {
  def this(function: () => R)(implicit r: Manifest[R]) = this("", function)(r)
  def name: String = aName
  def returnType = r.toString
  def parameterTypes: Seq[String] = Nil
  def apply(values: Any*): Any = function.apply()
}