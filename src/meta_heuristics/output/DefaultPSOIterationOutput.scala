package meta_heuristics.output

import meta_heuristics.particle_swarm_optimization.particle.Particle
import com.typesafe.scalalogging.slf4j.StrictLogging

/**
 * @author Derek Hawker
 */
trait DefaultPSOIterationOutput[T] extends StrictLogging
{

   def printIteration(i: Int,
                      population: Array[Particle[T]],
                      scores: Array[Double],
                      globalBest: Particle[T],
                      globalBestScore: Double,
                      localBest: Particle[T],
                      localBestScore: Double)
   {
      logger.info("^iteration: " + i)
      val mean: Double = scores.sum / scores.length
      logger.info("\tmean: " + mean
         + ", std.dev: " + math.sqrt(scores.foldLeft(0.0)(
         (count, s) =>
            count + math.pow(s - mean, 2.0)) / scores.length))

      logger.info("\t[Global]Best score = %f %s".format(globalBestScore, globalBest.position))
      logger.info("\t[Local]Best  score = %f %s".format(localBestScore, localBest.position))

      /**
       * Nice to have for debugging.
       */
      //    population
      //      .foreach(p => {
      //      logger.info("\t" + p.position + " " + p.velocity + " score: " + Scoring.doubleColour17x17Scorer(
      //        p.position.asInstanceOf[INTraitSeql[Double]]))
      //    })
   }
}
