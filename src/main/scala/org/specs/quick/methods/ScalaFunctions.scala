package org.specs.quick.methods

trait ScalaFunctions[+T <: ScalaFunction] { outer =>
  /** @return the list of functions */
  def functions: Seq[T]
  /** @return a ScalaMethods object, with the union of both method sets */
  def ++[U <: ScalaFunction](other: ScalaFunctions[U]) = new ScalaFunctions[ScalaFunction] {
	def functions = outer.functions ++ other.functions
  }
}
trait ScalaFunctionsFactory { 
  implicit def toFunctions[T : Manifest, R : Manifest](f: T => R): ScalaFunctions[ScalaTupledFunction[T, R]] = new ScalaFunctions[ScalaTupledFunction[T, R]] {
	val functions: Seq[ScalaTupledFunction[T, R]] = List(new ScalaTupledFunction(f))
  }
}
object ScalaFunctionsFactory extends ScalaFunctionsFactory
