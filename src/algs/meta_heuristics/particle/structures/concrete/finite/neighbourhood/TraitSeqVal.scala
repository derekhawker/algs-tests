package algs.meta_heuristics.particle.structures.concrete.finite.neighbourhood

import algs.meta_heuristics.particle.structures.specification.finite.neighbourhood.FiniteNeighbourhoodTraitSeq

class TraitSeqVal[T <: AnyVal](xs: Array[T],
                               neighbourhood: Array[Array[T]])
  extends FiniteNeighbourhoodTraitSeq[T](xs, neighbourhood) {

  override def deepcopy(): TraitSeqVal[T] =
    new TraitSeqVal[T](xs.clone(), neighbourhood)

}


//class FeaturesHyperplane(weights: Array[(Boolean, Double)]) extends TraitSequence(weights)