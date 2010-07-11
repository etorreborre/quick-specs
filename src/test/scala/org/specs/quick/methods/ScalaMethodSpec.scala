package org.specs.quick.methods
import org.specs._
import org.specs.util._
import org.scalacheck._

class ScalaMethodSpec extends SpecificationWithJUnit with ScalaMethodsFactory with Sugar with DataTables {
  val methodsForObject = ScalaMethods.create(new java.lang.Object)
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
    "have parameters types which are the type of its instance and the parameter types of its java method" in {
      equalsMethod.parameterTypes must_== List("Object", "java.lang.Object")
    }
    "have a toString method returning the method name" in {
      equalsMethod.toString must_== "equals"
    }
    "have an apply method applying a list of values to the method" in {
      class Adder {
        def addNone = "hello"
        def addOne(o: String) = "hello " + o
        def add1(i: Int) = "hello " + i
      }
      val adder = new Adder
      "if the ScalaMethods object has been created from a type, the values must provide an instance as the first value" >> {
        val addNoneMethod = ScalaMethods.create(adder).get("addNone")
        addNoneMethod.apply(adder) must_== "hello"
      }
      "if the ScalaMethods object has been created from a type, the values must provide an instance and other parameters" >> {
        val addOneMethod = ScalaMethods.create(adder).get("addOne")
        addOneMethod.apply(adder, "world") must_== "hello world"
      }
      "the passed values can be AnyVal too" >> {
        val add1Method = ScalaMethods.create(adder).get("add1")
        add1Method.apply(adder, 1) must_== "hello 1"
      }
    }
  }
  "A ScalaMethods object" can {
    "be tagged so that only accepted methods are returned with the get method" in {
      val methods = ScalaMethods.create(new Object).accept("toString")
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
  "The method name and parameter types can be extracted from a String with bracket" >> {
	import ScalaMethods._
	"when no type is specified: method" >> {
	  extractNameAndTypes("method") must_== ("method", Nil)
	}
	"when one type is specified: method[Int]" >> {
	  extractNameAndTypes("method[Int]") must_== ("method", List("Int"))
	}
	"when 2 types are specified: method[Int, String]" >> {
	  extractNameAndTypes("method[Int, String]") must_== ("method", List("Int", "String"))
	}
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
