package org.specs2
package collection
import CartesianProduct._
import org.scalacheck.Arbitrary
import org.scalacheck.Gen._

class CartesianProductSpec extends Specification with ScalaCheck { def is =

  "It is possible to get the cartesian product of several sequences"                                                    ! e1^
  "If one of the input sequence is empty, the result is empty"                                                          ! e2^
  "Each element of a list must be present in each of the results"                                                       ! e3^
                                                                                                                        end

  def product(input: String, output: String) = {
    def split(s: String) = s.split("\n").toSeq.map(_.split(",").toSeq)
    cartesianProduct(split(input)) must_== split(output)
  }
  
  def e1 = product(
     in ("1,2",
         "3,4"),
     out("1,3",
         "1,4",
         "2,3",
         "2,4"))

  def e2 = check { (l1: List[Int], l2: List[Int], l3: List[Int]) =>
    val input = Seq(l1, l2, l3)
    cartesianProduct(input) must be_===(Seq()).when(input.exists(_.isEmpty))
  }

  implicit val size = set(maxSize -> 3)
  
  def e3 = checkResult { (l1: List[Int], l2: List[Int], l3: List[Int]) =>
    val input = Seq(l1, l2, l3)
    val result = cartesianProduct(input)
    input.forall(_.forall(a => result.exists(r => r.contains(a)) || result.isEmpty))
  }

  def in(s: String*) = s.mkString("\n")
  def out(s: String*) = s.mkString("\n")
}