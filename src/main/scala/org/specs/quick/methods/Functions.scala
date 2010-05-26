package org.specs.quick

trait Functions {
  implicit def pipe[A](a: =>A): Pipe[A] = new Pipe(a)
  implicit def pipe2[A, B](a: =>A, b: =>B): Pipe[(A, B)] = new Pipe((a, b))
  class Pipe[A](a: =>A) {
	def |>[B](f: A => B) = f(a)
  }
}