package com.derek.algs.structures.concrete.infinite.neighbourhood

import com.derek.algs.structures.specification.finite.neighbourhood.FiniteNeighbourhoodTraitSeq
import com.derek.algs.structures.specification.TraitSeq

class INTraitSeqVal[T <: AnyVal](xs: Array[T])
  extends TraitSeq[T] with Serializable {

  override def iterator: Iterator[T] =
    xs.iterator

  override def update(index: Int,
                      value: T): Unit =
    xs(index) = value

  override def apply(index: Int): T =
    xs(index)

  override def length: Int =
    xs.length

  override def deepcopy(): TraitSeq[T] =
    new INTraitSeqVal(xs.clone())

  override def randNeighbourhoodMove(move: Int): T =
    throw new RuntimeException("Not implemented")

  override def bestNeighbourhoodMove(move: Int,
                                     scorer: (TraitSeq[T]) => Double): (TraitSeq[T], Double) =
    throw new RuntimeException("Not implemented")
}


//class FeaturesHyperplane(weights: Array[(Boolean, Double)]) extends TraitSequence(weights)