package com.derek.algs

import com.derek.algs.structures.specification.TraitSeq
import com.derek.algs.particle.swarm.optimization.Particle
import com.derek.algs.util.ExecutableAlgorithm

/**
 *
 * @param population Starting population.
 * @param velocityFollow
 * @param globalOptimumFollow
 * @param localOptimumFollow
 * @param numIterations
 * @param updateVelocity
 * @param endOfIterationCondition
 * @param iterationOutputPrinter
 * @param scorer
 * @tparam T Conforms to the type of TraitSeq
 *
 * @author Derek Hawker
 */
class ParticleSwarmOptimization[T](val population: Array[Particle[T]],
                                   val velocityFollow: Double,
                                   val globalOptimumFollow: Double,
                                   val localOptimumFollow: Double,
                                   val numIterations: Int,
                                   updateVelocity: (Particle[T], Double, Double, Double, Particle[T], TraitSeq[T] => Double) => Particle[T],
                                   endOfIterationCondition: (Int, Array[Particle[T]], Particle[T], Double, Particle[T], Double) => Boolean,
                                   iterationOutputPrinter: (Int, Array[Particle[T]], Array[Double], Particle[T], Double, Particle[T], Double) => Unit,
                                   scorer: TraitSeq[T] => Double) extends ExecutableAlgorithm[T] {

  //assert(localOptimumFollow >= 0.0 && localOptimumFollow <= 1.0)
  //assert(globalOptimumFollow >= 0.0 && globalOptimumFollow <= 1.0)

  override def execute(): TraitSeq[T] = {
    // Need a starting global best
    val scores = population.map(p => scorer(p.position))

    val startingGlobalBest = population.tail.zip(scores.tail)
      .foldLeft((population.head, scores.head))(
        scoreParticle)


    val results = innerExecute(startingGlobalBest)
    val pop = results._1
    val bestParticle = results._2._1

    bestParticle.position
  }


  private def innerExecute(startingGlobalBest: (Particle[T], Double)):
  (Array[Particle[T]], (Particle[T], Double)) = {

    Array.range(0, numIterations)
      .foldLeft((population, startingGlobalBest))(
        (state, i) => {
          val pop = state._1
          val globalBest = state._2._1
          val globalBestScore = state._2._2


          val newpop = pop
            .par.map(p => {
            updateVelocity(p, velocityFollow, globalOptimumFollow, localOptimumFollow, globalBest,
              scorer)
          }).seq.toArray

          val scores = newpop.map(p => scorer(p.position))

          val bestParticle = newpop.tail.zip(scores.tail)
            .foldLeft((newpop.head, scores.head))(
              scoreParticle)

          iterationOutputPrinter(i, newpop, scores, globalBest, globalBestScore, bestParticle._1,
            bestParticle._2)


          /** **************************************************************************************
           Early exit if meeting certain conditions */
          val canContinue = endOfIterationCondition(i, newpop, globalBest,
            globalBestScore, bestParticle._1, bestParticle._2)
          if (!canContinue)
            return if (bestParticle._2 > globalBestScore)
                     (newpop, bestParticle)
                   else
                     (newpop, state._2)

          /** *************************************************************************************/

          if (bestParticle._2 > globalBestScore)
            (newpop, bestParticle)
          else
            (newpop, state._2)
        })
  }

  /**
   * Used in a fold to evaluate a population of particles to find the particle with the highest
   * score
   *
   * @param highestScoringParticle Tuple2(_1: particle, _2: its score). 
   * @param currentParticle
   * @return highest scoring (particle, score) pair
   */
  def scoreParticle(highestScoringParticle: (Particle[T], Double),
                    currentParticle: (Particle[T], Double)): (Particle[T], Double) = {
    val bestScore = highestScoringParticle._2
    val currScore = currentParticle._2

    if (currScore > bestScore)
      currentParticle
    else
      highestScoringParticle
  }
}
