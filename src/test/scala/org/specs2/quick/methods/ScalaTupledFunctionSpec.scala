package org.specs2
package quick
package methods

class ScalaTupledFunctionSpec extends Specification { def is =

  "a Scala tupled function with no parameter should"                                                                    ^
	  "return the proper type for its return value"                                                                       ! nop.e1^
	  "have no parameter types"                                                                                           ! nop.e2^
	  "be able to apply an empty list of values"                                                                          ! nop.e3^
	  "return the same value even if a parameter is applied to the function"                                              ! nop.e4^
                                                                                                                        p^
  "a Scala tupled function with one parameter should"                                                                   ^
	  "return the proper type for its return value"                                                                       ! onep.e1^
	  "return the proper types for its parameter"                                                                         ! onep.e2^
	  "apply the passed parameters to the underlying function"                                                            ! onep.e3^
	  "throw an exception if the wrong parameter types are applied"                                                       ! onep.e4^
	  "rethrow the exception if the underlying function throws an exception"                                              ! onep.e5^
                                                                                                                        p^
  "a Scala tupled function with 2 parameters should"                                                                    ^
	  "return the proper type for its return value"                                                                       ! twop.e1^
	  "return the proper types for its parameter"                                                                         ! twop.e2^
	  "apply the passed parameters to the underlying function"                                                            ! twop.e3^
	  "throw an exception if the wrong parameter types are applied"                                                       ! twop.e4^
	  "rethrow the exception if the underlying function throws an exception"                                              ! twop.e5^
	  "show its parameters as a function call: f(a, b)"                                                                   ! twop.e6^
                                                                                                                        end

  object nop {
	  val f = new ScalaZeroArgFunction[String]("name", () => "a")
	  def e1 = f.returnType must_== "java.lang.String"
	  def e2 = f.parameterTypes must_== Nil
    def e3 = f.apply(Nil) must_== "a"
    def e4 = f.apply(List(1)) must_== "a"
  }

  object onep {
	  val f = new ScalaTupledFunction((i: Int) => "a")
	  def e1 = f.returnType must_== "java.lang.String"
	  def e2 = f.parameterTypes must_== List("Int")
	  def e3 = f.apply(1) must_== "a"
	  def e4 = f.apply("") must throwA[ScalaFunctionApplyException]
	  def e5 = {
   	  val f = new ScalaTupledFunction((i: Int) => {error("boom"); ""})
 	    f.apply(1) must throwA[RuntimeException]
	  }  
  }

  object twop {
    import Function._
	  val function = (i: Int, j: String) => 0.0
	  val f = new ScalaTupledFunction("f", function.tupled)
	  def e1 = f.returnType must_== "Double"
	  def e2 = f.parameterTypes must_== List("Int", "java.lang.String")
	  def e3 = f.apply(1, "hello") must_== 0.0
	  def e4 = f.apply("", 2) must throwA[ScalaFunctionApplyException]
	  def e5 = {
   	  val f = new ScalaTupledFunction(((i: Int, j: String) => {error("boom"); ""}).tupled)
 	    f.apply(1, "") must throwA[RuntimeException]
	  }  
	  def e6 = f.show(List("1", "a")) must_== "f(1, a)"
  }
}