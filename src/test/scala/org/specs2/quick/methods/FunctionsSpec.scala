package org.specs2
package quick
package methods

class FunctionsSpec extends Specification { def is =

  "This is just some code to generate the toTuple method"                                                               ^ toTuple

  def toTuple = {
	  "def toTuple(list: Seq[Any]): Any = {\n" +
      "  if (list.size == 1)\n    list(0)\n" +
          (2 to 22).map { (i: Int) =>
            "  else if (list.size == "+(i)+")\n" +
            (0 to i-1).map("list("+_+")").mkString("    (",", ",")")
          }.mkString("\n") +
      "}"
  }
}