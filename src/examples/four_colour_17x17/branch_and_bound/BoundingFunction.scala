package examples.four_colour_17x17.branch_and_bound

import meta_heuristics.structures.specification.TraitSeq
import optimization.BranchAndBound

/**
 * @author Derek Hawker
 */
trait BoundingFunction extends BranchAndBound[Int]
{
   final def boundingTrait(ts: TraitSeq[Int],
                     branchLevel: Int,
                     decisionVariableValue: Int): TraitSeq[Int] = {
      val updatedSol = ts.deepcopy()
      updatedSol(branchLevel) = variableBounds(branchLevel)(decisionVariableValue)

      updatedSol
   }

}
