package org.specs.quick.expression
import org.specs._
import org.specs.util._

class TypesMatcherSpec extends SpecificationWithJUnit with TypesMatcher with DataTables {
  "The following must match" in {
	 "t1"      			| "t2"     				|
	 "MyClass" 			! "MyClass" 			|
	 "List[String]" 	! "List[Object]"		|
	 "java.lang.String" ! "java.lang.Object" 	|
	 "Int" 				! "java.lang.Object"	| 	
	 "int" 				! "Int"				 	|> { (t1, t2) =>
	   typesMatch(t1, t2) aka (t1, t2).toString must beTrue
	 }
  }
}