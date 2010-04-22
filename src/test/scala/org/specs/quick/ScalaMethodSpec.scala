package org.specs.quick
import org.specs._
import org.specs.util._

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
      case class Adder(i: Int) {
        def addOne(l: Int) = i + l
      }
      val m3 = Methods.create[Adder].get("addOne")
      m3.apply(Adder(2), 3) must_== 5
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
