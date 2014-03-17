package com.derek.algs.structures.concrete.infinite.neighbourhood

import com.derek.algs.structures.specification.inifinite.neighbourhood.{InfiniteNeighbourhoodTraitSeq, RandIntNeighbourhoodSearch}

/**
 *
 * @author Derek Hawker
 * @param xs
 * @param ranges
 */
class TraitSeqVal(override val xs: Array[Int],
                  val ranges: Array[(Int, Int)])
  extends InfiniteNeighbourhoodTraitSeq[Int](xs) with RandIntNeighbourhoodSearch {

  override def deepcopy(): TraitSeqVal =
    new TraitSeqVal(xs.clone(), ranges) {
    }

}
