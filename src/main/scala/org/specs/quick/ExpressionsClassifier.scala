package org.specs.quick

trait ExpressionsClassifier {
  def classify(expressions: List[Expression]) = {
    new EquivalenceClass(expressions).partition(4)
  }
}