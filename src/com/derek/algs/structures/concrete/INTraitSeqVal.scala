package com.derek.algs.structures.concrete

import com.derek.algs.structures.specification.{TraitSeq, InfiniteNeighbourhoodTraitSeq}

/**
 * @author Derek Hawker
 */
abstract class INTraitSeqVal[T <: AnyVal](override val xs: Array[T])
  extends InfiniteNeighbourhoodTraitSeq[T](xs) with RandNeighbourhoodSearch{

  override def deepcopy(): INTraitSeqVal[T] =
    new INTraitSeqVal[T](xs.clone())
  //
  //override def bestNeighbourhoodMove(move: Int,
  //                                   scorer: (TraitSeq[T]) => Double): (TraitSeq[T], Double) = ???
  //
  //override def randNeighbourhoodMove(move: Int): T = ???
}
