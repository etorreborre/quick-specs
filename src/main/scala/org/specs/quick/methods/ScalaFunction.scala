package org.specs.quick.methods

/**
 * This trait represent functions which can have parameters
 * applied to. The function declares the types of the parameters it is
 * accepting as strings
 */
trait ScalaFunction {
  def name: String
  def returnType: String
  def parameterTypes: Seq[String]
  def apply(values: Any*): Any
}