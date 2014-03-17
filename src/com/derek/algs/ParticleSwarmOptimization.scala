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
                                   //endOfIterationCondition: (Int, Array[TraitSeq[T]], Array[Double]) => Boolean,
                                   iterationOutputPrinter: (Int, Array[Particle[T]], Particle[T], Double) => Unit,
                                   scorer: TraitSeq[T] => Double) extends ExecutableAlgorithm[T] {

  //assert(localOptimumFollow >= 0.0 && localOptimumFollow <= 1.0)
  //assert(globalOptimumFollow >= 0.0 && globalOptimumFollow <= 1.0)

  override def execute(): TraitSeq[T] = {
    val results = innerExecute()

    results._2.position
  }


  private def innerExecute(): (Array[Particle[T]], Particle[T]) = {
    Array.range(0, numIterations)
      .foldLeft((population, population.head))(
        (state, i) => {
          val pop = state._1
          val globalBest = state._2
          val globalBestScore = scorer(globalBest.position)

          val newpop = pop
            .map(p => {
            updateVelocity(p, velocityFollow, globalOptimumFollow, localOptimumFollow, globalBest, scorer)
          })

          val bestParticle = newpop.foldLeft((globalBest, globalBestScore))(
            (best, curr) => {
              val bestScore = best._2
              val currScore = scorer(curr.position)

              if (currScore > bestScore)
                (curr, currScore)
              else
                best
            })

          iterationOutputPrinter(i, newpop, bestParticle._1, bestParticle._2)

          (newpop, bestParticle._1)
        })
  }
}
