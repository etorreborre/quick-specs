package org.specs.quick

trait ExpressionsClassifier {
  val classify = classifyExpressions _
  private def classifyExpressions(equivalenceClass: EquivalenceClass) = {
    equivalenceClass.partition(4)
  }
}