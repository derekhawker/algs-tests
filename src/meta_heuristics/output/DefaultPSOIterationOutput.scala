package meta_heuristics.output

import meta_heuristics.particle_swarm_optimization.particle.Particle

/**
 * @author Derek Hawker
 */
trait DefaultPSOIterationOutput[T]
{

   def printIteration(i: Int,
                      population: Array[Particle[T]],
                      scores: Array[Double],
                      globalBest: Particle[T],
                      globalBestScore: Double,
                      localBest: Particle[T],
                      localBestScore: Double)
   {
      println("^iteration: " + i)
      val mean: Double = scores.sum / scores.length
      println("\tmean: " + mean
         + ", std.dev: " + math.sqrt(scores.foldLeft(0.0)(
         (count, s) =>
            count + math.pow(s - mean, 2.0)) / scores.length))

      println("\t[Global]Best score = %f %s".format(globalBestScore, globalBest.position))
      println("\t[Local]Best  score = %f %s".format(localBestScore, localBest.position))

      /**
       * Nice to have for debugging.
       */
      //    population
      //      .foreach(p => {
      //      println("\t" + p.position + " " + p.velocity + " score: " + Scoring.doubleColour17x17Scorer(
      //        p.position.asInstanceOf[INTraitSeql[Double]]))
      //    })
   }
}
