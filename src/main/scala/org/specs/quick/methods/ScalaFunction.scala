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

//class ScalaTypedFunction[a1, b](aName: String, function: a1 => b, params: Seq[String], aReturnType: String) 
//   extends ScalaFunction with Function1[a1, b] {
//  import Function._
//  def name: String = aName
//  def returnType = aReturnType
//  def parameterTypes: Seq[String] = params
//  def apply(values: a1): b = function.apply(values) 
//}