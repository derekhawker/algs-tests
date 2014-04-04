package com.derek.algs.particle.swarm.optimization

import com.derek.algs.structures.specification.TraitSeq
import scala.util.Random

/**
 *
 * @author Derek Hawker
 *
 * @param position
 * @param velocity
 * @tparam T
 */
class Particle[T](val position: TraitSeq[T],
                  val velocity: TraitSeq[T],
                  val localBest: TraitSeq[T]) extends Iterable[(T, T, T)] with Serializable {

  override def iterator: Iterator[(T, T, T)] = new Iterator[(T, T, T)] {
    var i = 0

    override def next(): (T, T, T) = {
      val index = i
      i += 1

      (position(index), velocity(index), localBest(index))
    }

    override def hasNext: Boolean =
      i < position.length
  }


  /**
   * Return a deep copy of the particle. Localbest is returned as a shallow copy because it
   * should already be a copy
   * @return deep copy of this particle
   */
  def deepcopy(): Particle[T] =

    new Particle[T](position.deepcopy(), velocity.deepcopy(), localBest)
}


object Particle {
//  def updateVelocity[T](position: T,
//                        velocity: T,
//                        localBestPosition: T,
//                        globalBestPosition: T,
//                        velocityFollow: Double,
//                        globalOptimumFollow: Double,
//                        localOptimumFollow: Double,): T = {
//
//  }
//
//  def updatePosition[T](position: T,
//                        velocity: T,
//                        positionBounds: (T, T)): T = {
//
//  }
def updateVelocity(position: Double,
                      velocity: Double,
                      localBestPosition: Double,
                      globalBestPosition: Double,
                      velocityFollow: Double,
                      globalOptimumFollow: Double,
                      localOptimumFollow: Double): Double =
  (velocityFollow * velocity
    + globalOptimumFollow * Random.nextDouble() * (globalBestPosition - position)
    + Random.nextDouble() * localOptimumFollow * (localBestPosition - position))


  /**
   * Update position, implementing wraparound when position exceeds allowable boundariess
   *
   * @param position
   * @param velocity
   * @param positionBounds
   * @return
   */
  def updatePosition(position: Double,
                        velocity: Double,
                        positionBounds: (Double, Double)): Double = {

    val lowerBounds = positionBounds._1
    val upperBounds = positionBounds._2
    ((position + velocity) % upperBounds) + lowerBounds
  }
}