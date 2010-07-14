package org.specs.quick.methods
import java.lang.reflect.Method
import org.specs.specification.Tagged
import org.specs.Sugar
import reflect.{NameTransformer, ClassManifest}
import org.specs.util.Property

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
  protected val method: Method
  lazy val declaringClass: Class[_] = method.getDeclaringClass
  lazy val returnType: String = method.getReturnType.getName
  
  private var methodParameterTypes: Property[List[String]] = Property(method.getParameterTypes.map(_.getName).toList)
  def parameterTypes: List[String] = methodParameterTypes.apply()
  def parameterTypes_=(types: List[String]) = {
	methodParameterTypes = Property(types)
	this
  }
  
  lazy val methodName = NameTransformer.decode(method.getName)

  def apply(values: Any*): Any = applyValues(values.toSeq)
  def apply(values: List[Any]): Any = applyValues(values)
  def applyValues(values: Seq[Any]): Any

  override def toString = methodName
  tag(methodName)
}

/**
 * This class stores a Method and the instance on which this Method should be applied. 
 *
 */
case class InstanceMethod(instance: AnyRef, method: Method) extends ScalaMethod {
  def applyValues(values: Seq[Any]): Any = {
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
case class ClassMethod(instanceType: String, method: Method) extends ScalaMethod {
  override def parameterTypes: List[String] = instanceType :: super.parameterTypes
  def applyValues(values: Seq[Any]): Any = {
    require (values.size >= 1)
    if (values.size == 1)
     method.invoke(values(0)) 
    else {
	  val parameters = values.drop(1).toArray.map(_.asInstanceOf[Object])
	  method.invoke(values(0).asInstanceOf[Object], parameters:_*)
    }
  }
}