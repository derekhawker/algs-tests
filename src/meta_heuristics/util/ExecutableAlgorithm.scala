package meta_heuristics.util

import meta_heuristics.structures.specification.TraitSeq

/**
 * @author Derek Hawker
 */
trait ExecutableAlgorithm[@specialized(Char, Double, Int) T]
{
   def execute(): Option[TraitSeq[T]]
}
