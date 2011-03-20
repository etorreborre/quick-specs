package org.specs2.quick

trait Functions {
  implicit def pipe[A](a: =>A): Pipe[A] = new Pipe(a)
  implicit def pipe2[A, B](a: =>A, b: =>B): Pipe[(A, B)] = new Pipe((a, b))
  class Pipe[A](a: =>A) {
	def |>[B](f: A => B) = f(a)
  }
  /**
   * @see the generation script in FunctionsSpec.scala
   */
def toTuple(list: Seq[Any]): Any = {
  if (list.size == 1)
    list(0)
  else if (list.size == 2)
    (list(0), list(1))
  else if (list.size == 3)
    (list(0), list(1), list(2))
  else if (list.size == 4)
    (list(0), list(1), list(2), list(3))
  else if (list.size == 5)
    (list(0), list(1), list(2), list(3), list(4))
  else if (list.size == 6)
    (list(0), list(1), list(2), list(3), list(4), list(5))
  else if (list.size == 7)
    (list(0), list(1), list(2), list(3), list(4), list(5), list(6))
  else if (list.size == 8)
    (list(0), list(1), list(2), list(3), list(4), list(5), list(6), list(7))
  else if (list.size == 9)
    (list(0), list(1), list(2), list(3), list(4), list(5), list(6), list(7), list(8))
  else if (list.size == 10)
    (list(0), list(1), list(2), list(3), list(4), list(5), list(6), list(7), list(8), list(9))
  else if (list.size == 11)
    (list(0), list(1), list(2), list(3), list(4), list(5), list(6), list(7), list(8), list(9), list(10))
  else if (list.size == 12)
    (list(0), list(1), list(2), list(3), list(4), list(5), list(6), list(7), list(8), list(9), list(10), list(11))
  else if (list.size == 13)
    (list(0), list(1), list(2), list(3), list(4), list(5), list(6), list(7), list(8), list(9), list(10), list(11), list(12))
  else if (list.size == 14)
    (list(0), list(1), list(2), list(3), list(4), list(5), list(6), list(7), list(8), list(9), list(10), list(11), list(12), list(13))
  else if (list.size == 15)
    (list(0), list(1), list(2), list(3), list(4), list(5), list(6), list(7), list(8), list(9), list(10), list(11), list(12), list(13), list(14))
  else if (list.size == 16)
    (list(0), list(1), list(2), list(3), list(4), list(5), list(6), list(7), list(8), list(9), list(10), list(11), list(12), list(13), list(14), list(15))
  else if (list.size == 17)
    (list(0), list(1), list(2), list(3), list(4), list(5), list(6), list(7), list(8), list(9), list(10), list(11), list(12), list(13), list(14), list(15), list(16))
  else if (list.size == 18)
    (list(0), list(1), list(2), list(3), list(4), list(5), list(6), list(7), list(8), list(9), list(10), list(11), list(12), list(13), list(14), list(15), list(16), list(17))
  else if (list.size == 19)
    (list(0), list(1), list(2), list(3), list(4), list(5), list(6), list(7), list(8), list(9), list(10), list(11), list(12), list(13), list(14), list(15), list(16), list(17), list(18))
  else if (list.size == 20)
    (list(0), list(1), list(2), list(3), list(4), list(5), list(6), list(7), list(8), list(9), list(10), list(11), list(12), list(13), list(14), list(15), list(16), list(17), list(18), list(19))
  else if (list.size == 21)
    (list(0), list(1), list(2), list(3), list(4), list(5), list(6), list(7), list(8), list(9), list(10), list(11), list(12), list(13), list(14), list(15), list(16), list(17), list(18), list(19), list(20))
  else if (list.size == 22)
    (list(0), list(1), list(2), list(3), list(4), list(5), list(6), list(7), list(8), list(9), list(10), list(11), list(12), list(13), list(14), list(15), list(16), list(17), list(18), list(19), list(20), list(21))}
}
object Functions extends Functions