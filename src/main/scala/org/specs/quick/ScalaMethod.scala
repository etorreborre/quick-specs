package org.specs.quick
import java.lang.reflect.Method
import org.specs.specification.Tagged
import org.specs.Sugar
import org.specs.util.Classes
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
  val declaringClass: Class[_] = method.getDeclaringClass
  def getType: String = method.getReturnType.getName
  def getParameterTypes: List[String] = method.getParameterTypes.map(_.getName).toList
  def methodName = NameTransformer.decode(method.getName)
  override def toString = methodName
  def apply(values: Any*): Any = apply(values.toList)
  def apply(values: List[Any]): Any
  tag(methodName)
}

/**
 * This class is a ScalaMethod for the method of a Scala object. 
 *
 * It stores the object that should be used to invoke the method.
 */
case class ObjectMethod(instance: AnyRef, method: Method) extends ScalaMethod {
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
case class InstanceMethod(method: Method) extends ScalaMethod {
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


/**
 * This class holds a list of methods, belonging to the same class, for which equations will be generated.
 * 
 * It is tagged so that some methods can be accepted or rejected from the set by their name
 */
class Methods(declaringClass: Class[_], val methods: List[ScalaMethod]) extends Tagged {
  def getOwn: List[ScalaMethod] = {
    val selected = if (declaringClass.getSuperclass != null)
                      methods filterNot(m => inheritedMethods.map(_.methodName) contains m.methodName)
                   else 
					  methods
    selected.filter(_.isAccepted)
  }
  private def inheritedMethods = Methods.create(declaringClass.getSuperclass).methods
  /** @return the list of accepted methods */
  def get: List[ScalaMethod] = methods.filter(_.isAccepted)
  /** 
   * @return a method using its name 
   * @throw a MatchException if the method can't be found
   */
  def get(methodName: String): ScalaMethod = methods.find(_.methodName == methodName).get
  
  /** 
   * @return Some(method) using the method name. None if it can't be found
   */
  def find(name: String): Option[ScalaMethod] = get.find(_.methodName == name)
  /** 
   * @return In this Tagged object the components are the list of methods
   */
  override def taggedComponents = methods 
}
/**
 * Companion object to create Methods instances from an instance
 */
object Methods extends Classes {
  /**
   * create a Methods object from an object
   *
   */
  def create(a: AnyRef): Methods = {
    new Methods(a.getClass, a.getClass.getMethods.toList.map(ObjectMethod(a, _)))
  }
  def create[A <: AnyRef](implicit m: ClassManifest[A]): Methods = {
    val c = m.erasure
    new Methods(c, c.getMethods.toList.map(InstanceMethod(_)))
  }
}
/**
 * Transforms any object to a Methods object containing a list of ScalaMethods
 */
trait MethodsFactory {
  implicit def toMethods(a: AnyRef) = Methods.create(a)
}
