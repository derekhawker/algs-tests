package com.derek.algs

import com.derek.algs.structures.specification.TraitSeq
import com.derek.algs.particle.swarm.optimization.Particle
import com.derek.algs.util.ExecutableAlgorithm

/**
 * @author Derek Hawker
 */
class ParticleSwarmOptimization[T](val population: Array[Particle[T]],
                                   val velocityFollow: Double,
                                   val globalOptimumFollow: Double,
                                   val localOptimumFollow: Double,
                                   val numIterations: Int,
                                   updateVelocity: (Particle[T], Double, Double, Double, Particle[T], TraitSeq[T] => Double) => Particle[T],
                                   endOfIterationCondition: (Int, Particle[T], Double, Particle[T], Double) => Boolean,
                                   iterationOutputPrinter: (Int, Array[Particle[T]], Particle[T], Double, Particle[T], Double) => Unit,
                                   scorer: TraitSeq[T] => Double) extends ExecutableAlgorithm[T] {

  //assert(localOptimumFollow >= 0.0 && localOptimumFollow <= 1.0)
  //assert(globalOptimumFollow >= 0.0 && globalOptimumFollow <= 1.0)

  override def execute(): TraitSeq[T] = {
    val results = innerExecute()
    val pop = results._1
    val bestParticle = results._2

    bestParticle.position
  }


  private def innerExecute(): (Array[Particle[T]], Particle[T]) = {
    val bestParticle = population.tail
      .foldLeft((population.head, scorer(population.head.position)))(
        scoreParticle)


    Array.range(0, numIterations)
      .foldLeft((population, population.head))(// doesn't
        (state, i) => {
          val pop = state._1
          val globalBest = state._2
          val globalBestScore = scorer(globalBest.position)


          val newpop = pop
            .par.map(p => {
            updateVelocity(p, velocityFollow, globalOptimumFollow, localOptimumFollow, globalBest,
              scorer)
          }).seq.toArray

          val bestParticle = newpop.tail
            .foldLeft((newpop.head, scorer(newpop.head.position)))(
              scoreParticle)

          iterationOutputPrinter(i, newpop, globalBest, globalBestScore, bestParticle._1,
            bestParticle._2)


          /** **************************************************************************************
           Early exit if meeting certain conditions */
          val canContinue = endOfIterationCondition(i, globalBest, globalBestScore, bestParticle._1,
            bestParticle._2)
          if (!canContinue)
            return if (bestParticle._2 > globalBestScore)
                     (newpop, bestParticle._1)
                   else
                     (newpop, globalBest)

          /** *************************************************************************************/

          if (bestParticle._2 > globalBestScore)
            (newpop, bestParticle._1)
          else
            (newpop, globalBest)

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
                    currentParticle: Particle[T]): (Particle[T], Double) = {
    val bestScore = highestScoringParticle._2
    val currScore = scorer(currentParticle.position)

    if (currScore > bestScore)
      (currentParticle, currScore)
    else
      highestScoringParticle
  }
}
