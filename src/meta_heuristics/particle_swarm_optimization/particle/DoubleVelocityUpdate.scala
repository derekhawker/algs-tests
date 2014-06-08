package meta_heuristics.particle_swarm_optimization.particle

import scala.util.Random

/**
 * @author Derek Hawker
 */
trait DoubleVelocityUpdate
{
   final def updateVelocity(position: Double,
                      velocity: Double,
                      localBestPosition: Double,
                      globalBestPosition: Double,
                      velocityFollow: Double,
                      globalOptimumFollow: Double,
                      localOptimumFollow: Double): Double =
      (velocityFollow * velocity
         + globalOptimumFollow * Random.nextDouble() * (globalBestPosition - position)
         + Random.nextDouble() * localOptimumFollow * (localBestPosition - position))
}
