package com.derek.algs.structures.concrete

import com.derek.algs.structures.specification.{TraitSeq, InfiniteNeighbourhoodTraitSeq}
import com.derek.algs.structures.inifinite.neighbourhood.RandIntNeighbourhoodSearch

/**
 * @author Derek Hawker
 */
class INTraitSeqVal(override val xs: Array[Int],
                                           val ranges: Array[(Int, Int)])
  extends InfiniteNeighbourhoodTraitSeq[Int](xs) with RandIntNeighbourhoodSearch{

  override def deepcopy(): INTraitSeqVal =
    new INTraitSeqVal(xs.clone(), ranges){
    }

}
