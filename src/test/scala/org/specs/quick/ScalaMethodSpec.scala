package org.specs.quick
import org.specs._
import org.specs.util._
import org.scalacheck._

class ScalaMethodSpec extends SpecificationWithJUnit with MethodsFactory with Sugar with DataTables {
  "A ScalaMethod" can {
    "be created from an object" in {
      Methods.create(new Object).methods must have size(9)
    }
    "be created from a java method" in {
      Methods.create(new Object).methods must have (_.methodName == "toString")
    }
    val m = Methods.create[Object].get("toString")
    val m2 = Methods.create[Object].get("equals")
    "have a type which is the return type of its java method" in {
      m.getType must_== "java.lang.String"
    }
    "have parameters types which are the parameter types of its java method" in {
      m2.getParameterTypes must_== List("java.lang.Object")
    }
    "have a toString method returning the method name" in {
      m2.toString must_== "equals"
    }
    "have an apply method applying a list of values to the method" in {
      case class Adder() {
        def addOne(o: String) = "hello " + o
      }
      val m3 = Methods.create[Adder].get("addOne")
      m3.apply("world") must_== "hello world"
    }
  }
  "A Methods object" can {
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
