package meta_heuristics.output

import meta_heuristics.structures.specification.TraitSeq
import java.util.PriorityQueue
import optimization.branch_and_bound.OpenSolution

/**
 * @author Derek Hawker
 */
trait DefaultBranchAndBoundIterationOutput[T]
{
   private final val UPDATE_FREQ = 10000

   def printIteration(iteration: Int,
                      level: Int,
                      openSolutions: PriorityQueue[OpenSolution[T]],
                      incumbent: Option[TraitSeq[T]],
                      incumbentScore: Double,
                      openedSolutions: Array[TraitSeq[T]],
                      openedSolutionsScore: Array[Double]): Unit =
   {
      if ((iteration % UPDATE_FREQ) != 0)
         return

      println("^iteration, level, open sol'ns: " + iteration + ", " + level + ", " + openSolutions
         .size())

      println("\tIncumbent: score = %f %s".format(incumbentScore, incumbent))
      println("\tOpened nodes:")
      openedSolutions.zip(openedSolutionsScore)
         .foreach(zp => {
         val sol = zp._1
         val score = zp._2
         println("\t\tscore = %f %s".format(score, sol))
      })

   }
}
