package org.specs2
package quick

import expression._
import methods._

class QuickSpecs2Spec extends Specification with QuickSpecs with SampleLists { def is =
                                                                                                                        """
  Generation of equations for:

    - the ++ function on lists
    - 2 list variables: xs and ys

        val xs = Variable[List[Int]]("xs")
        val ys = Variable[List[Int]]("ys")

    - a constant: nil


                                                                                                                        """^
  "The equations for adding any list to an empty list"                                                                  ! e1^
                                                                                                                        end


  def e1 = {
	  val nil = constant("nil", Lists.nil[Int])
    val xs = Variable[List[Int]]("xs")
    val ys = Variable[List[Int]]("ys")
      implicit val args = Args(combineDepth = 1)
      val equations = quick(Lists.find("++").toList, xs, nil)
      "[xs == lists.++(nil, xs)]" +
      "[xs == lists.++(xs, nil)]"
      equations.split("\n").toList must have size(2)
  }
}