package org.specs.quick
import java.lang.reflect.Method
import org.specs.specification.Tagged
import org.specs.Sugar
import org.specs.util.Classes
import reflect.{NameTransformer, ClassManifest}

trait MethodsFactory {
  implicit def toMethods(a: AnyRef) = Methods.create(a)
}
case class Methods(methods: List[ScalaMethod]) extends Tagged {
  def getOwn: List[ScalaMethod] = {
    val superClass = methods(0).m.getDeclaringClass
    val selected = if (superClass != null)
                     methods filterNot(Methods.create(methods(0).instance, superClass).methods contains _)
                   else methods
    methods.filter(_.isAccepted)
  }
  def get: List[ScalaMethod] = methods.filter(_.isAccepted)
  def get(methodName: String) = methods.find(_.methodName == methodName).get
  def methodByName(name: String) = get.find(_.methodName == name)
  override def taggedComponents = methods 
}
case object Methods extends Classes {
  def create(a: AnyRef): Methods = create(a, a.getClass)
  def create[A <: AnyRef](implicit m: ClassManifest[A]): Methods = {
    create(super.tryToCreateObject(m.erasure.getName).get, m.erasure)
  }
  def create(a: AnyRef, c: Class[_]) = Methods(c.getMethods.toList.map(ScalaMethod(a, _)))
}
case class ScalaMethod(instance: AnyRef, m: Method) extends Tagged with Sugar {
  def getType = m.getReturnType.getName
  def getParameterTypes = m.getParameterTypes.toList.map(_.getName)
  def methodName = NameTransformer.decode(m.getName)
  override def toString = methodName
  def apply(values: Any*): Any = apply(values.toList)
  def apply(values: List[Any]): Any = {
    val valuesMinusInstance = values.filterNot(_ == instance)
    if (valuesMinusInstance.isEmpty) {
     m.invoke(instance) 
    }
    else {
	  val parameters = valuesMinusInstance.toArray.map(_.asInstanceOf[Object])
	  m.invoke(instance, parameters:_*)
    }
  }
  tag(methodName)
}
