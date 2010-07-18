package org.specs.quick.methods
import Function._

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
  private def toTuple(list: Seq[Any]): Any = {
	if (list.size == 1)
	  list(0)
	else if (list.size == 2)
	  (list(0), list(1))
	else if (list.size == 3)
	  (list(0), list(1), list(2))
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