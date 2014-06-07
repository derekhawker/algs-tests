package meta_heuristics.output

import meta_heuristics.structures.specification.TraitSeq
import optimization.BranchAndBound.{Solution, BranchAndBoundNode}
import java.util.PriorityQueue

/**
 * @author Derek Hawker
 */
trait DefaultBranchAndBoundIterationOutput[T]
{
   def printIteration(iteration: Int,
                      level: Int,
                      openSolutions: PriorityQueue[Solution[T]],
                      incumbent: Option[TraitSeq[T]],
                      incumbentScore: Double,
                      openedSolutions: Array[BranchAndBoundNode[T]],
                      openedSolutionsScore: Array[Double]): Unit =
   {

      println("^iteration, level, open sol'ns: " + iteration + ", " + level + ", " + openSolutions.size())

      println("\tIncumbent: score = %f %s".format(incumbentScore, incumbent))
      println("\tOpened nodes:")
      openedSolutions.zip(openedSolutionsScore)
         .foreach(zp => {
         val sol = zp._1
         val score = zp._2
         println("\t\tscore = %f %s".format(score, sol.solution))
      })

   }
}
