package org.specs.quick.classify
import org.specs.quick.expression._
import org.specs.quick.methods._

/**
 * This trait implements the classification strategy once the 
 * methods and variables have been combined.
 * 
 * At the moment the strategy is very naive. For each input equivalence class, it partitions it
 * 4 times by evaluating its expressions and dividing the expressions by distinct results
 */
trait ExpressionsClassifier {
  implicit val partitionsNumber = 4
  val classify = classifyExpressions _
  
  private def classifyExpressions(equivalenceClass: EquivalenceClass)(implicit number: Int) = equivalenceClass.partition(number)
}