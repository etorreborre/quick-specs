package org.specs.quick.methods
import org.specs._

class ScalaTupledFunctionSpec extends SpecificationWithJUnit {
  "a Scala tupled function with no parameter" should {
	val f = new ScalaZeroArgFunction[String]("name", () => "a")
	"return the proper type for its return value" in {
	  f.returnType must_== "java.lang.String"
	}  
	"have no parameter types" in {
	  f.parameterTypes must_== Nil
	}  
	"be able to apply an empty list of values" in {
	  f.apply(Nil) must_== "a"
	}  
	"return the same value even if a parameter is applied to the function" in {
	  f.apply(List(1)) must_== "a"
	}  
  }
  "a Scala tupled function with one parameter" should {
	val f = new ScalaTupledFunction((i: Int) => "a")
	"return the proper type for its return value" in {
	  f.returnType must_== "java.lang.String"
	}  
	"return the proper types for its parameter" in {
	  f.parameterTypes must_== List("Int")
	}  
	"apply the passed parameters to the underlying function" in {
	  f.apply(1) must_== "a"
	}  
	"throw an exception if the wrong parameter types are applied" in {
	  f.apply("") must throwA[ScalaFunctionApplyException]
	}  
	"rethrow the exception if the underlying function throws an exception" in {
   	  val f = new ScalaTupledFunction((i: Int) => {error("boom"); ""})
 	  f.apply(1) must throwA[RuntimeException]
	}  
  }
  import Function._
  "a Scala tupled function with 2 parameters" should {
	val function = (i: Int, j: String) => 0.0
	val f = new ScalaTupledFunction(function.tupled)
	"return the proper type for its return value" in {
	  f.returnType must_== "Double"
	}  
	"return the proper types for its parameter" in {
	  f.parameterTypes must_== List("Int", "java.lang.String")
	}  
	"apply the passed parameters to the underlying function" in {
	  f.apply(1, "hello") must_== 0.0
	}  
	"throw an exception if the wrong parameter types are applied" in {
	  f.apply("", 2) must throwA[ScalaFunctionApplyException]
	}  
	"rethrow the exception if the underlying function throws an exception" in {
   	  val f = new ScalaTupledFunction(((i: Int, j: String) => {error("boom"); ""}).tupled)
 	  f.apply(1, "") must throwA[RuntimeException]
	}  
  }
}