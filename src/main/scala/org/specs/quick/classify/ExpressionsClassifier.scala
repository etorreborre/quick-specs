package org.specs.quick.classify
import org.specs.quick.expression._
import org.specs.quick.equality._

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
  
  private def classifyExpressions(combined: CombinedExpressions)(implicit number: Int): List[Equality[Expression]] = {
    val p = new EquivalenceClass(combined.expressions, combined.variables).partition(number)
    p.flatMap(_.equalities).sortBy(_.toString.size)
  }
}