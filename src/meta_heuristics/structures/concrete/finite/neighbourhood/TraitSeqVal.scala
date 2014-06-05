package meta_heuristics.structures.concrete.finite.neighbourhood

import meta_heuristics.structures.specification.finite.neighbourhood.FiniteNeighbourhoodTraitSeq
import java.util

class TraitSeqVal[T <: AnyVal](xs: Array[T],
                               neighbourhood: Array[Array[T]])
  extends FiniteNeighbourhoodTraitSeq[T](xs, neighbourhood)
{

  override def deepcopy(): TraitSeqVal[T] =
    new TraitSeqVal[T](xs.clone(), neighbourhood)

}


//class FeaturesHyperplane(weights: Array[(Boolean, Double)]) extends TraitSequence(weights)