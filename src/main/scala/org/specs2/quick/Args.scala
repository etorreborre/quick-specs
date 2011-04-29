package org.specs2
package quick


case class Args(combineDepth: Int = 2, verbose: Verbose = Verbose())
case class Verbose(combine: Boolean = false,
                   classify: Boolean = false,
                   congruent: Boolean = false,
                   flatten: Boolean = false,
                   prune: Boolean = false)