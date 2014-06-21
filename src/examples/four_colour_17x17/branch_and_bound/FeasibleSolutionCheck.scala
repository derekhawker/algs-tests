package examples.four_colour_17x17

import meta_heuristics.structures.specification.TraitSeq
import optimization.BranchAndBound

/**
 * @author Derek Hawker
 */
trait FeasibleSolutionCheck extends BranchAndBound[Int]
{
   /**
    *
    * @param ts
    * @return
    */
   final def isFeasible(ts: TraitSeq[Int]): Boolean =
      ts.forall(seq => seq < 4)
}
