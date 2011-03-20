package org.specs2.quick
package classify
import expression._
import equality._
import expression.ValuedExpression

/**
 * This trait implements the classification strategy once the 
 * methods and variables have been combined.
 * 
 * At the moment the strategy is very naive. For each input equivalence class, it partitions it
 * 4 times by evaluating its expressions and dividing the expressions by distinct results
 */
trait ExpressionsClassifier {
  implicit val partitionsNumber = 4
  val classify = (c: CombinedExpressions) => classifyExpressions(c)(partitionsNumber) 
  
  private def classifyExpressions(combined: CombinedExpressions)(number: Int = partitionsNumber): List[Equality[ValuedExpression]] = {
	val partitions = new EquivalenceClass(combined.expressions, combined.variables).partition(number)
	val result = partitions.flatMap(_.equalities).sortBy(_.toString.size).distinct
	println("partitions "+partitions.mkString("\n"))
	println("classified expressions "+result)
	result
  }
}