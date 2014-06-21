package examples.four_colour_17x17.branch_and_bound

import meta_heuristics.structures.specification.TraitSeq
import optimization.BranchAndBound

import scala.util.Random

/**
 * @author Derek Hawker
 */
trait BoundingFunction extends BranchAndBound[Int]
{
   def boundingTrait(ts: TraitSeq[Int],
                     branchLevel: Int,
                     decisionVariableValue: Int): TraitSeq[Int] =
   {
      val updatedSol = ts.deepcopy()
      updatedSol(branchLevel) = variableBounds(branchLevel)(decisionVariableValue)

      var i = 0
      while (i < branchLevel) {
         if (Random.nextDouble() < 0.01) {
            val ri = Random.nextInt(variableBounds(i).length)
            updatedSol(i) = variableBounds(i)(ri)
         }

         i += 1
      }

      updatedSol
   }
}
