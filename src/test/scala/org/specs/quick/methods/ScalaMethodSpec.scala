package org.specs.quick.methods
import org.specs._
import org.specs.util._
import org.scalacheck._

class ScalaMethodSpec extends SpecificationWithJUnit with ScalaMethodsFactory with Sugar with DataTables {
  val methodsForObject = ScalaMethods.create[Object]
  "A ScalaMethods object grouping several ScalaMethods" can {
    "be created from a scala object" in {
      object AScalaObject
      ScalaMethods.create(AScalaObject).methods must have size(9)
    }
    "be created from a type" in {
      methodsForObject.methods must have (_.methodName == "toString")
    }
  }
  "A ScalaMethod object" should {
    val toStringMethod: ScalaMethod = methodsForObject.get("toString")
    val equalsMethod: ScalaMethod = methodsForObject.get("equals")
    "have a type which is the return type of its associated java method" in {
      toStringMethod.returnType must_== "java.lang.String"
    }
    "have parameters types which are the parameter types of its java method" in {
      equalsMethod.parameterTypes must_== List("java.lang.Object")
    }
    "have a toString method returning the method name" in {
      equalsMethod.toString must_== "equals"
    }
    "have an apply method applying a list of values to the method" in {
      case class Adder() {
        def addOne(o: String) = "hello " + o
        def add1(i: Int) = "hello " + i
      }
      "if the ScalaMethods object has been created from a type, the values must provide an instance as the first value" >> {
        val addOneMethod = ScalaMethods.create[Adder].get("addOne")
        addOneMethod.apply(Adder(), "world") must_== "hello world"
      }
      "if the ScalaMethods object has been created from an instance, the values must provide only the parameters" >> {
        val addOneMethod = ScalaMethods.create(new Adder).get("addOne")
        addOneMethod.apply("world") must_== "hello world"
      }
      "the passed values can be AnyVal too" >> {
        val add1Method = ScalaMethods.create(new Adder).get("add1")
        add1Method.apply(1) must_== "hello 1"
      }
    }
  }
  "A ScalaMethods object" can {
    "be tagged so that only accepted methods are returned with the get method" in {
      val methods = ScalaMethods.create[Object].accept("toString")
      methods.methods must have(_.methodName == "toString")
      methods.methods must notHave(_.methodName == "equals")
    }
    "return only the object own methods and not the inherited ones with the getOwn method" in {
      ScalaMethods.create(new { def add = () }).getOwn.apply(0).methodName must_== "add"
    }
  }
  "The name of a ScalaMethod" should {
    "replace obfuscated characters when displaying the method name" in {
      class o { override def toString = "object" }
      "object"                     | "methodName" |>
        new o { def ++ = () }      ! "++"         |
        new o { def -- = () }      ! "--"         | { (o: Object, m: String) =>
          ScalaMethods.create(o).getOwn(0).methodName must_== m
      }
    }
  }
}
trait InstanceMethods extends GenerationParams { 
  implicit val arbInstanceMethods: Arbitrary[InstanceMethod]= Arbitrary(InstanceMethod(new Object, classOf[Object].getDeclaredMethods().apply(0)))
}
trait ClassMethods extends GenerationParams {
  implicit val arbClassMethods: Arbitrary[ClassMethod]= Arbitrary(ClassMethod(classOf[Object].getDeclaredMethods().apply(0)))
}
trait AnyScalaMethods extends InstanceMethods with ClassMethods {
  implicit val anyScalaMethod: Arbitrary[ScalaMethod]= Arbitrary(Gen.oneOf(arbInstanceMethods.arbitrary, arbClassMethods.arbitrary))
}
