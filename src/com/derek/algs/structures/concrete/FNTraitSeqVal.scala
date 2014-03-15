package com.derek.algs.structures.concrete

import com.derek.algs.structures.specification.FiniteNeighbourhoodTraitSeq

class FNTraitSeqVal[T <: AnyVal](override val xs: Array[T],
                                    override val neighbourhood: Array[Array[T]])
  extends FiniteNeighbourhoodTraitSeq[T](xs, neighbourhood) {

  override def deepcopy(): FNTraitSeqVal[T] =
    new FNTraitSeqVal[T](xs.clone(), neighbourhood)

}


//class FeaturesHyperplane(weights: Array[(Boolean, Double)]) extends TraitSequence(weights)