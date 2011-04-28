package org.specs2
package quick
package methods
import ScalaMethods._
import matcher._
import org.scalacheck._

class ScalaMethodSpec extends Specification with ScalaMethodsFactory with DataTables { def is =

  "A ScalaMethods object grouping several ScalaMethods can"                                                             ^
    "be created from a scala object"                                                                                    ! ms.e1^
    "be created from a type"                                                                                            ! ms.e2^
                                                                                                                        p^
  "A ScalaMethods object can"                                                                                           ^
    "be tagged so that only accepted methods are returned with the get method"                                          ! ms.e3^
    "return only the object own methods and not the inherited ones with the getOwn method"                              ! ms.e4^
                                                                                                                        p^
  "A ScalaMethod object should"                                                                                         ^
    "have a type which is the return type of its associated Method object"                                              ! m.e1^
    "have parameters types which are the type of its instance and the parameter types of its Method object"             ! m.e2^
    "have a toString method returning the method name"                                                                  ! m.e3^
    "have an apply method applying a list of values to the method"                                                      ! m.e4^
      "if the ScalaMethods object has been created from a type"                                                         ^
        "the values must provide an instance as the first value"                                                        ^
        "the values must provide an instance and other parameters"                                                      ! m.e5^
        "the passed values can be AnyVal too"                                                                           ! m.e6^bt(2)^
    "show its parameters application as a method call: instance.method(a, b)"                                           ! m.e7^
                                                                                                                        endp^
  "The name of a ScalaMethod should"                                                                                    ^
    "replace obfuscated characters when displaying the method name"                                                     !{
      "object"                       | "index" | "methodName" |>
      new Calculator                 ! 0       ! "++"         |
      new Calculator                 ! 1       ! "--"         | { (o, i, m) =>
        create(o).getOwn(i).methodName must_== m
      }
                                                                                                                        }^
  "The method name and parameter types can be extracted from a String with brackets"                                    ^
  	"when no type is specified: method"                                                                                 ! names.e1^
  	"when one type is specified: method[Int]"                                                                           ! names.e2^
  	"when 2 types are specified: method[Int, String]"                                                                   ! names.e3^
                                                                                                                        end

  object ms {
    def e1 = create(AScalaObject).methods must have size(9)
    def e2 = methodsForObject.methods must have (_.methodName == "toString")
    def e3 = {
      val methods = create(new Object).find("toString").get
      methods.methods(0) must (be_==("toString")) ^^ ((_.methodName))
      methods.methods(0) must not (be_==("equals")) ^^ ((_).methodName)
    }
    def e4 = create(new { def add = () }).getOwn.apply(0).methodName must_== "add"
  }

  object m {
    val toStringMethod: ScalaMethod = methodsForObject.get("toString")
    val equalsMethod: ScalaMethod = methodsForObject.get("equals")

    def e1 = toStringMethod.returnType must_== "java.lang.String"
    def e2 = equalsMethod.parameterTypes must_== List("java.lang.Object", "java.lang.Object")
    def e3 = equalsMethod.toString must_== "equals"

    def e4 = create(adder).get("addNone").apply(adder) must_== "hello"
    def e5 = create(adder).get("addOne").apply(adder, "world") must_== "hello world"
    def e6 = create(adder).get("add1").apply(adder, 1) must_== "hello 1"
    def e7 = equalsMethod.show(List("instance", "1")) must_== "instance.equals(1)"
  }

  object names {
    def e1 = extractNameAndTypes("method") must_== ("method", Nil)
    def e2 = extractNameAndTypes("method[Int]") must_== ("method", List("Int"))
    def e3 = extractNameAndTypes("method[Int, String]") must_== ("method", List("Int", "String"))
  }

  val methodsForObject = create(new java.lang.Object)
  val adder = new Adder
  object AScalaObject

  class Adder {
    def addNone = "hello"
    def addOne(o: String) = "hello " + o
    def add1(i: Int) = "hello " + i
  }

  class Calculator extends AnyRef {
    override def toString = "calculator"
    def ++ = ()
    def -- = ()
  }

}
trait InstanceMethods extends GenerationParams { 
  implicit val arbInstanceMethods: Arbitrary[InstanceMethod]= Arbitrary(InstanceMethod(new Object, classOf[Object].getDeclaredMethods().apply(0)))
}
trait ClassMethods extends GenerationParams {
  implicit val arbClassMethods: Arbitrary[ClassMethod]= Arbitrary(ClassMethod("java.lang.Object", classOf[Object].getDeclaredMethods().apply(0)))
}
trait AnyScalaMethods extends InstanceMethods with ClassMethods {
  implicit val anyScalaMethod: Arbitrary[ScalaMethod]= Arbitrary(Gen.oneOf(arbInstanceMethods.arbitrary, arbClassMethods.arbitrary))
}
