package algs.meta_heuristics.particle.util

import algs.meta_heuristics.particle.structures.specification.TraitSeq

/**
 * @author Derek Hawker
 */
trait ExecutableAlgorithm[T] {
  def execute(): TraitSeq[T]
}
