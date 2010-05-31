package org.specs.quick.methods
import java.lang.reflect.Method
import org.specs.specification.Tagged
import org.specs.Sugar
import reflect.{NameTransformer, ClassManifest}


/**
 * This trait represents a method with:
 * - a return type
 * - the types of its parameters
 * - a method name (as readable in the source code, not as encoded in the bytecode)
 *
 * It is also a Tagged object, tagged with the method name, so that it can be included or excluded from 
 * a set of other methods.
 *
 * The apply method can be used to pass parameter values and invoke the method
 */
trait ScalaMethod extends Tagged {
  val method: Method
  lazy val declaringClass: Class[_] = method.getDeclaringClass
  lazy val returnType: String = method.getReturnType.getName
  lazy val parameterTypes: List[String] = method.getParameterTypes.map(_.getName).toList
  lazy val methodName = NameTransformer.decode(method.getName)

  def apply(values: Any*): Any = apply(values.toList)
  def apply(values: List[Any]): Any
  override def toString = methodName

  tag(methodName)
}

/**
 * This class stores a Method and the instance on which this Method should be applied. 
 *
 */
case class InstanceMethod(instance: AnyRef, method: Method) extends ScalaMethod {
  def apply(values: List[Any]): Any = {
    if (values.isEmpty)
     method.invoke(instance) 
    else {
	  val parameters = values.toArray.map(_.asInstanceOf[Object])
	  method.invoke(instance, parameters:_*)
    }
  }
}
/**
 * This class is a ScalaMethod for the method of an instance of a class. 
 *
 * The prerequisite of the apply method is that the first value of the list of values
 * is an instance of the class that can be used to invoke the method
 */
case class ClassMethod(method: Method) extends ScalaMethod {
  def apply(values: List[Any]): Any = {
    require (values.size >= 1)
    if (values.size == 1)
     method.invoke(values(0)) 
    else {
	  val parameters = values.drop(1).toArray.map(_.asInstanceOf[Object])
	  method.invoke(values(0), parameters:_*)
    }
  }
}