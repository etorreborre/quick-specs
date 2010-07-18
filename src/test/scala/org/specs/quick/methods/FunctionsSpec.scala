package org.specs.quick.methods
import org.specs._

class FunctionsSpec extends SpecificationWithJUnit with Sugar {
  "The toTuple method can be generated" >> {
	val result = 
	  "def toTuple(list: Seq[Any]): Any = {\n" +
      "  if (list.size == 1)\n    list(0)\n" +
          (2 to 22).map { (i: Int) =>
            "  else if (list.size == "+(i)+")\n" +
            (0 to i-1).map("list("+_+")").mkString("    (",", ",")")
          }.mkString("\n") +
      "}"
    result.isExpectation
  }
}