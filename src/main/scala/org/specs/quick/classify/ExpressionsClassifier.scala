package org.specs.quick.classify
import org.specs.quick.expression._
import org.specs.quick.methods._

trait ExpressionsClassifier {
  val classify = classifyExpressions _
  private def classifyExpressions(equivalenceClass: EquivalenceClass) = {
    equivalenceClass.partition(4)
  }
}