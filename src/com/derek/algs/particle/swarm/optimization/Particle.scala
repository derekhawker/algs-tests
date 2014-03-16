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
                  val localBest: TraitSeq[T])

object Particle {
  def updateVelocityDouble(particle: Particle[Double],
                           velocityFollow: Double,
                           globalOptimumFollow: Double,
                           localOptimumFollow: Double,
                           globalBest: Particle[Double],
                           scorer: TraitSeq[Double] => Double): Particle[Double] = {

    val newTs = particle.position.zip(particle.velocity).map(pair => pair._1 + pair._2).toArray

    val newVel = particle.velocity.deepcopy()
    newVel.zipWithIndex
      .foreach(pair => {
      val v = pair._1
      val i = pair._2
      val lb: TraitSeq[Double] = particle.localBest
      newVel(i) = (velocityFollow * v
        + globalOptimumFollow * Random.nextDouble() * (globalBest.position(i) - particle.position(
        i))
        + Random.nextDouble() * localOptimumFollow * (lb(i) - particle.position(i)))
    })

    val newPos = particle.position.deepcopy()
    (0 until particle.position.length)
      .foreach(i => {
      newPos(i) = particle.position(i) + newVel(i)
    })

    val oldscore = scorer(particle.localBest)
    val newscore = scorer(newPos)

    new Particle[Double](newPos, newVel, if (newscore > oldscore)
                                           newPos
                                         else
                                           particle.localBest)
  }

  def updateVelocityInt(particle: Particle[Int],
                        velocityFollow: Double,
                        globalOptimumFollow: Double,
                        localOptimumFollow: Double,
                        globalBest: Particle[Int],
                        scorer: TraitSeq[Int] => Double): Particle[Int] = {

    val newVel = particle.velocity.deepcopy()
    val pos: TraitSeq[Int] = particle.position

    newVel.zipWithIndex
      .foreach(pair => {
      val v = pair._1
      val i = pair._2
      val lb: TraitSeq[Int] = particle.localBest

      newVel(i) = math.max(-3,
        math.min(3,
          (velocityFollow * v
            + Random.nextDouble() * globalOptimumFollow * (globalBest.position(i) - pos(i))
            + Random.nextDouble() * localOptimumFollow * (lb(i) - pos(i))).toInt))
    })

    val newPos = pos.deepcopy()
    (0 until pos.length)
      .foreach(i => {
      newPos(i) = Math.max(0, Math.min(3, pos(i) + newVel(i)))
    })

    val oldscore = scorer(particle.localBest)
    val newscore = scorer(newPos)

    new Particle[Int](newPos, newVel, if (newscore > oldscore)
                                        newPos
                                      else
                                        particle.localBest)
  }
}