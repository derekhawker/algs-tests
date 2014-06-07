package meta_heuristics.output

import meta_heuristics.structures.specification.TraitSeq
import optimization.BranchAndBound.BranchAndBoundNode

/**
 * @author Derek Hawker
 */
trait DefaultBranchAndBoundIterationOutput[T]
{
   def printIteration(iteration: Int,
                      level: Int,
                      incumbent: Option[TraitSeq[T]],
                      incumbentScore: Double,
                      openedSolutions: Array[BranchAndBoundNode[T]],
                      openedSolutionsScore: Array[Double]): Unit =
   {

      println("^iteration, level: " + iteration + ", " + level)

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
