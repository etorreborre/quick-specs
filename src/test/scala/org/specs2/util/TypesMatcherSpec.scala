package org.specs2.util
import org.specs2.matcher._
import org.specs2.mutable._

class TypesMatcherSpec extends Specification with TypesMatcher with DataTables {
  "The following must match" in {
	  "t1"      			    || "t2"     				  |
	  "MyClass" 			    !! "MyClass" 			    |
	  "List[String]"      !! "List[Object]"		  |
	  "String"            !! "java.lang.String" |
	  "java.lang.String"  !! "java.lang.Object" |
	  "Int" 				      !! "java.lang.Object"	|
	  "int" 				      !! "Int"				 	    |> { (t1, t2) =>
	   typesMatch(t1, t2) aka (t1, t2).toString must beTrue
	 }
  }
  "Two equal list of parameters must match" >> {
	  typesMatch(List("t1", "t2"), List("t1", "t2")) must beTrue
  }
  "Two lists of parameters with different sizes must not match" >> {
	  typesMatch(List("t1", "t2"), List("t1")) must beFalse
  }
}