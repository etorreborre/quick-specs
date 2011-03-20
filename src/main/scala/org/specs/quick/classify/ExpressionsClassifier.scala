package org.specs.quick.classify
import org.specs.quick.expression._
import org.specs.quick.equality._
import org.specs.log._

/**
 * This trait implements the classification strategy once the 
 * methods and variables have been combined.
 * 
 * At the moment the strategy is very naive. For each input equivalence class, it partitions it
 * 4 times by evaluating its expressions and dividing the expressions by distinct results
 */
trait ExpressionsClassifier extends Log {
  implicit val partitionsNumber = 4
  val classify = (c: CombinedExpressions) => classifyExpressions(c)(partitionsNumber) 
  
  private def classifyExpressions(combined: CombinedExpressions)(number: Int = partitionsNumber): List[Equality[ValuedExpression]] = {
	val partitions = new EquivalenceClass(combined.expressions, combined.variables).partition(number)
	val result = partitions.flatMap(_.equalities).sortBy(_.toString.size).distinct
	debug("partitions "+partitions.mkString("\n"))
	debug("classified expressions "+result)
	result
  }
}