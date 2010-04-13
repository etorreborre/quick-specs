package org.specs.quick
import java.lang.reflect.Method
import scala.reflect.Manifest
import org.specs.specification.Tagged

trait MethodsFactory {
  implicit def toMethods(a: AnyRef) = Methods.create(a)
}
case class Methods(methods: List[ScalaMethod]) extends Tagged {
  def get: List[ScalaMethod] = methods.filter(_.isAccepted)
  def get(methodName: String) = methods.find(_.methodName == methodName).get
  def methodByName(name: String) = get.find(_.methodName == name)
  override def taggedComponents = methods 
}
case object Methods {
  def create(a: AnyRef): Methods = create(a.getClass)
  def create[A <: AnyRef](implicit m: Manifest[A]): Methods = create(m.erasure)
  def create(c: Class[_]) = Methods(c.getMethods.toList.map(ScalaMethod(_)))
}
case class ScalaMethod(m: Method) extends Tagged with Sugar {
  def getType = m.getReturnType.getName
  def getParameterTypes = m.getParameterTypes.toList.map(_.getName)
  def methodName = m.getName.replace("$plus", "+")
  override def toString = methodName
  def apply(values: Any*): Any = apply(values.toList)
  def apply(values: List[Any]): Any = {
    val parameters = values.drop(1)
    val a = parameters.toArray
    m.invoke(values(0), a.map(_.asInstanceOf[Object]):_*)
  }
  tag(methodName)
}
