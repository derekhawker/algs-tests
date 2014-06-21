package meta_heuristics.output

import meta_heuristics.structures.specification.TraitSeq
import java.util.PriorityQueue
import optimization.branch_and_bound.OpenSolution
import com.typesafe.scalalogging.slf4j.StrictLogging

/**
 * @author Derek Hawker
 */
trait DefaultBranchAndBoundIterationOutput[T] extends StrictLogging
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

      logger.info("^iteration, level, open sol'ns: " + iteration + ", " + level + ", " + openSolutions
         .size())

      logger.info("\tIncumbent: score = %f %s".format(incumbentScore, incumbent))
      logger.info("\tOpened nodes:")
      openedSolutions.zip(openedSolutionsScore)
         .foreach(zp => {
         val sol = zp._1
         val score = zp._2
         logger.info("\t\tscore = %f %s".format(score, sol))
      })

   }
}
