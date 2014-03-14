package com.derek.algs.structures


class TraitSequenceVal[T <: AnyVal](override val xs: Array[T],
                                    override val neighbourhood: Array[Array[T]])
  extends FiniteNeighbourhoodTraitSequence[T](xs, neighbourhood) {

  override def deepcopy(): TraitSequenceVal[T] =
    new TraitSequenceVal[T](xs.clone(), neighbourhood)

}


//class FeaturesHyperplane(weights: Array[(Boolean, Double)]) extends TraitSequence(weights)