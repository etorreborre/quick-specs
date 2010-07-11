package org.specs.quick.methods
import org.specs.util.Classes
import org.specs.specification.Tagged

/**
 * This class holds a list of methods, belonging to the same class, for which equations will be generated.
 * 
 * It is tagged so that some methods can be accepted or rejected from the set by their name
 */
class ScalaMethods(declaringClass: Class[_], private val scalaMethods: List[ScalaMethod]) extends Tagged {
  import ScalaMethods._
  
  def getOwn: List[ScalaMethod] = {
    val selected = if (declaringClass.getSuperclass != null)
                      methods filterNot(m => inheritedMethods.map(_.methodName) contains m.methodName)
                   else 
					  methods
    selected.filter(_.isAccepted)
  }
  private def inheritedMethods = ScalaMethods.create(declaringClass.getSuperclass).methods
  /** @return the list of accepted methods */
  def methods: List[ScalaMethod] = scalaMethods.filter(_.isAccepted)
  /** 
   * @return a method using its name 
   * @throw a MatchException if the method can't be found
   */
  def get(methodName: String): ScalaMethod = methods.find(_.methodName == methodName).get
  
  /** 
   * @return Some(method) using the method name. None if it can't be found
   */
  def find(name: String): Option[ScalaMethod] = methods.find(_.methodName == name)
  /** 
   * @return In this Tagged object the components are the list of methods
   */
  override def taggedComponents = methods
  /**
   * get methods, adding typing information
   */
  def select(methodNames: String*) = {
	new ScalaMethods(declaringClass, selectMethods(methodNames.toList))
  }
  private def selectMethods(methodNames: List[String]) = {
	methodNames.flatMap { name =>
	  val (selectedName, types) = extractNameAndTypes(name)
	  scalaMethods.find(_.methodName == selectedName).map(m => m.parameterTypes = types)
	}
  }
}
/**
 * Companion object to create Methods instances from an instance
 */
object ScalaMethods {
  import Classes._
  /**
   * create a ScalaMethods object from an object
   */
  def create[T <: AnyRef](a: T)(implicit m: ClassManifest[T]): ScalaMethods = {
    new ScalaMethods(a.getClass, a.getClass.getMethods.toList.map(ClassMethod(m.erasure.getName, _)))
  }
  private val MethodNamePattern = """(.*)\[(.*)\]""".r
  private[methods] def extractNameAndTypes(methodName: String) = {
	if (methodName matches MethodNamePattern.toString) {
  	  val MethodNamePattern(selectedName, types) = methodName
	  (selectedName, types.split(",").map(_.trim).toList)
	} else
      (methodName, Nil: List[String])
  }
}
/**
 * Transforms any object to a Methods object containing a list of ScalaMethods
 */
trait ScalaMethodsFactory {
  implicit def toMethods[T <: AnyRef](a: T)(implicit m: ClassManifest[T]) = ScalaMethods.create(a)
  implicit def toScalaMethods(m: ScalaMethod) = new ScalaMethods(m.declaringClass, List(m))
  import org.scalacheck._
  def variable [A](name: String)(implicit m: ClassManifest[A], arb: Arbitrary[A], params: Gen.Params) = new ArbitraryVariable(name)(m, arb, params)
  def constant[A](name: String, v: A)(implicit m: ClassManifest[A]) = new Constant(name, v)(m)
  implicit def constant[A](v: A)(implicit m: ClassManifest[A]) = new Constant(v.toString, v)(m)

}
