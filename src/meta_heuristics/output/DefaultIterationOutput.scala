package meta_heuristics.output

import meta_heuristics.structures.specification.TraitSeq
import com.typesafe.scalalogging.slf4j.StrictLogging

/**
 * @author Derek Hawker
 */
trait DefaultIterationOutput[T] extends StrictLogging
{
   def printIteration(iteration: Int,
                      population: Array[TraitSeq[T]],
                      scores: Array[Double],
                      globalBest: TraitSeq[T],
                      globalBestScore: Double,
                      localBest: TraitSeq[T],
                      localBestScore: Double)
   {

      logger.info("^iteration: " + iteration)

      val mean: Double = scores.sum / scores.length
      logger.info("\tmean: " + mean
         + ", std.dev: " + math.sqrt(scores.foldLeft(0.0)(
         (count, s) =>
            count + math.pow(s - mean, 2.0)) / scores.length))

      logger.info("\t[Global]Best: score = %f %s".format(globalBestScore, globalBest))
      logger.info("\t[Local]Best:  score = %f %s".format(localBestScore, localBest))
   }
}





