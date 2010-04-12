package org.specs.quick
import java.lang.reflect.Method
import org.specs.specification.Tagged

trait MethodsFactory {
  implicit def toMethods(a: AnyRef) = Methods.create(a)
}
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
  def apply(values: List[Any]) = {} 
}
