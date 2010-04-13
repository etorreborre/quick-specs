package org.specs.quick
import org.specs._

class ScalaMethodSpec extends SpecificationWithJUnit with MethodsFactory with Sugar {
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
}
