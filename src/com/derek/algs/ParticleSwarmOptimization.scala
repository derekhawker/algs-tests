package com.derek.algs

import com.derek.algs.structures.TraitSeq
import com.derek.algs.particleswarmoptimization.Particle

/**
 * @author Derek Hawker
 */
class ParticleSwarmOptimization[T](val population: Array[Particle[T]],
                                   val globalOptimumFollow: Double,
                                   val localOptimumFollow: Double,
                                   val numIterations: Int,
                                   updateVelocity: (Particle[T], Double, Double, Particle[T], TraitSeq[T] => Double) => Particle[T],
                                   //endOfIterationCondition: (Int, Array[TraitSeq[T]], Array[Double]) => Boolean,
                                   //generationOutputPrinter: (Int, Array[TraitSeq[T]], Array[Double], (TraitSeq[T], Double), (TraitSeq[T], Double)) => Unit,
                                   scorer: TraitSeq[T] => Double) extends ExecutableAlgorithm[T] {

  assert(localOptimumFollow >= 0.0 && localOptimumFollow <= 1.0)
  assert(globalOptimumFollow >= 0.0 && globalOptimumFollow <= 1.0)

  override def execute(): TraitSeq[T] = {
    val results = innerExecute()

    null
  }


  private def innerExecute(): (Array[Particle[T]], Particle[T]) = {
    Array.range(0, numIterations)
      .foldLeft((population, population.head))(
        (state, i) => {
          val pop = state._1
          val globalBest = state._2

          val newpop = pop
            .map(p => {
            updateVelocity(p, globalOptimumFollow, localOptimumFollow, globalBest, scorer)
          })

          //TODO. incopmlete
          (newpop, newpop.head)
        })
  }


}
