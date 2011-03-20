package org.specs2
package quick

import mutable._
import expression._
import methods._

class QuickSpecsSpec extends Specification with QuickSpecs with SampleLists with SampleVariables {
  "creating quick specs" should {
	  val nil = constant("lists.nil()", Lists.nil)
    "return a list of equations for the most simple case" in {
      combineDepth(1)
      val equations = quick(Lists.find("\\+\\+").toList, lists, xs, nil).split("\n").toList
      equations must have size(2)
      "[xs == lists.++(lists.nil(), xs)]" +
      "[xs == lists.++(xs, lists.nil())]"
      success
    }
    "return a list of equations for a case with 2 variables and one operation" in {
      combineDepth(1)
      quick(Lists.find("\\+\\+\\+").toList, lists, xs, ys) must_== List(
        "[lists.+++(xs, ys) == lists.+++(ys, xs)]"
      ).mkString("\n")
    }
    "return a list of equations for a case with 2 variables and two operations" in {
      combineDepth(1)
      quick(Lists.find("\\+\\+").toList, lists, xs, ys, nil).split("\n").toList must have size(2)
      quick(Lists.find("\\+\\+").toList, lists, xs, ys, nil).toString must contain("[xs == lists.++(lists.nil(), xs)]") and
                  contain("[xs == lists.++(xs, lists.nil())]")
    }
  }
}