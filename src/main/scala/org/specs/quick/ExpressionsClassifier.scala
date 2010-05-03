package org.specs.quick

trait ExpressionsClassifier {
  def classify(equivalenceClass: EquivalenceClass) = {
    equivalenceClass.partition(4)
  }
}