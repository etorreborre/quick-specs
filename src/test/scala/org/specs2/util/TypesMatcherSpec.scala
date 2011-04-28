package org.specs2
package util
import matcher._

class TypesMatcherSpec extends Specification with TypesMatcher with DataTables { def is =

  "The following types must match"                                                                                      !{
	  "t1"      			    || "t2"     				  |
	  "MyClass" 			    !! "MyClass" 			    |
	  "List[String]"      !! "List[Object]"		  |
	  "String"            !! "java.lang.String" |
	  "java.lang.String"  !! "java.lang.Object" |
	  "Int" 				      !! "java.lang.Object"	|
	  "int" 				      !! "Int"				 	    |> { (t1, t2) => typesMatch(t1, t2) }
                                                                                                                        }^
  "Two equal lists of parameters must match"                                                                            ^
  { typesMatch(List("t1", "t2"), List("t1", "t2")) }                                                                    ^p^
  "Two lists of parameters with different sizes must not match"                                                         ^
  { !typesMatch(List("t1", "t2"), List("t1")) }                                                                         ^p^
  "Two empty lists of parameters must match"                                                                            ^
  { typesMatch(List(), List()) }                                                                                        ^p^
  "An empty list of parameters must not match a non-empty one"                                                          ^
  { !typesMatch(List(), List("t1")) }                                                                                   ^
	                              	                              	                              	                      end

}