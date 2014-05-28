package algs.meta_heuristics.util

import algs.meta_heuristics.structures.specification.TraitSeq

/**
 * @author Derek Hawker
 */
trait ExecutableAlgorithm[T] {
  def execute(): TraitSeq[T]
}
