package meta_heuristics.util

import meta_heuristics.structures.specification.TraitSeq

/**
 * @author Derek Hawker
 */
trait ExecutableAlgorithm[@specialized(Int, Double) T]
{
   def execute(): TraitSeq[T]
}
