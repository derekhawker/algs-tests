package com.derek.algs.structures.concrete.finite.neighbourhood

import com.derek.algs.structures.specification.finite.neighbourhood.FiniteNeighbourhoodTraitSeq

class TraitSeqVal[T <: AnyVal](override val xs: Array[T],
                                    override val neighbourhood: Array[Array[T]])
  extends FiniteNeighbourhoodTraitSeq[T](xs, neighbourhood) {

  override def deepcopy(): TraitSeqVal[T] =
    new TraitSeqVal[T](xs.clone(), neighbourhood)

}


//class FeaturesHyperplane(weights: Array[(Boolean, Double)]) extends TraitSequence(weights)