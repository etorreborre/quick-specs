package org.specs.quick
import org.specs._
import org.specs.util._
import org.scalacheck._

class ScalaMethodSpec extends SpecificationWithJUnit with MethodsFactory with Sugar with DataTables {
  val methodsForObject = Methods.create[Object]
  "A ScalaMethods object grouping several ScalaMethods" can {
    "be created from a scala object" in {
      object AScalaObject
      Methods.create(AScalaObject).methods must have size(9)
    }
    "be created from a type" in {
      methodsForObject.methods must have (_.methodName == "toString")
    }
  }
  "A ScalaMethod object" should {
    val toStringMethod: ScalaMethod = methodsForObject.get("toString")
    val equalsMethod: ScalaMethod = methodsForObject.get("equals")
    "have a type which is the return type of its associated java method" in {
      toStringMethod.getType must_== "java.lang.String"
    }
    "have parameters types which are the parameter types of its java method" in {
      equalsMethod.getParameterTypes must_== List("java.lang.Object")
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
        val addOneMethod = Methods.create[Adder].get("addOne")
        addOneMethod.apply(Adder(), "world") must_== "hello world"
      }
      "if the ScalaMethods object has been created from an instance, the values must provide only the parameters" >> {
        val addOneMethod = Methods.create(new Adder).get("addOne")
        addOneMethod.apply("world") must_== "hello world"
      }
      "the passed values can be AnyVal too" >> {
        val add1Method = Methods.create(new Adder).get("add1")
        add1Method.apply(1) must_== "hello 1"
      }
    }
  }
  "A ScalaMethods object" can {
    "be tagged so that only accepted methods are returned with the get method" in {
      val methods = Methods.create[Object].accept("toString")
      methods.get must have(_.methodName == "toString")
      methods.get must notHave(_.methodName == "equals")
    }
    "return only the object own methods and not the inherited ones with the getOwn method" in {
      Methods.create(new { def add = () }).getOwn.apply(0).methodName must_== "add"
    }
  }
  "The name of a ScalaMethod" should {
    "replace obfuscated characters when displaying the method name" in {
      class o { override def toString = "object" }
      "object"                     | "methodName" |>
        new o { def ++ = () }      ! "++"         |
        new o { def -- = () }      ! "--"         | { (o: Object, m: String) =>
          Methods.create(o).getOwn(0).methodName must_== m
      }
    }
  }
}
trait ObjectMethods extends GenerationParams { 
  implicit val arbObjectMethods: Arbitrary[ObjectMethod]= Arbitrary(ObjectMethod(new Object, classOf[Object].getDeclaredMethods().apply(0)))
}
trait InstanceMethods extends GenerationParams {
  implicit val arbInstanceMethods: Arbitrary[InstanceMethod]= Arbitrary(InstanceMethod(classOf[Object].getDeclaredMethods().apply(0)))
}
trait ScalaMethods extends ObjectMethods with InstanceMethods {
  implicit val anyScalaMethod: Arbitrary[ScalaMethod]= Arbitrary(Gen.oneOf(arbObjectMethods.arbitrary, arbInstanceMethods.arbitrary))
}
