package com.derek.algs.structures


class TraitSeqVal[T <: AnyVal](override val xs: Array[T],
                                    override val neighbourhood: Array[Array[T]])
  extends FiniteNeighbourhoodTraitSeq[T](xs, neighbourhood) {

  override def deepcopy(): TraitSeqVal[T] =
    new TraitSeqVal[T](xs.clone(), neighbourhood)

}


//class FeaturesHyperplane(weights: Array[(Boolean, Double)]) extends TraitSequence(weights)