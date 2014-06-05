package meta_heuristics

import meta_heuristics.structures.specification.TraitSeq
import meta_heuristics.particle_swarm_optimization.particle.Particle

/**
 * @author Derek Hawker
 */
trait IgnoredGeneticAlgorithmCondition[T]
{
   def checkForConvergence(generation: Int,
                           population: Array[TraitSeq[T]],
                           score: Array[Double]): Boolean =
      true
}


trait IgnoredPSOCondition[T]
{
   def checkForConvergence(iteration: Int,
                           population: Array[Particle[T]],
                           globalBest: Particle[T],
                           globalBestScore: Double,
                           localBest: Particle[T],
                           localBestScore: Double): Boolean =
      true
}
