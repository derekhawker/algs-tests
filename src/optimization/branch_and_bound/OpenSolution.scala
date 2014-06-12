package optimization.branch_and_bound

import meta_heuristics.structures.specification.TraitSeq

/**
 * @author Derek Hawker
 */
final case class OpenSolution[T](parentSolution: TraitSeq[T],
                                 boundingScore: Double,
                                 level: Int)
