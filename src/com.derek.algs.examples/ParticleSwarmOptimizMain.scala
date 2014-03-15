package com.derek.algs.examples

import com.derek.algs.ParticleSwarmOptimization
import com.derek.algs.util.TimedExecution
import com.derek.algs.structures.{TraitSeqVal, TraitSeq}
import scala.util.Random
import com.derek.algs.particleswarmoptimization.Particle

/**
 * @author Derek Hawker
 */
object ParticleSwarmOptimizMain {
  def main(args: Array[String]) {

    val localOptimumFollow = 0.1
    val globalOptimumFollow = 0.2
    val numIterations = 100
    val numPopulation = 20
    val numFeatures = 2

    val population = Array.range(0, numPopulation)
      .map(p => {
      val initWeights = Array.range(0, numFeatures)
        .map(m => Random.nextDouble())

      val initVelocity = Array.range(0, numFeatures)
        .map(m => Random.nextDouble())

      new Particle[Double](new TraitSeqVal[Double](initWeights, null),
        new TraitSeqVal[Double](initVelocity, null),
        new TraitSeqVal[Double](initWeights, null))
    })

    new TimedExecution().execute {
      val best = new ParticleSwarmOptimization[Double](population, globalOptimumFollow,
        localOptimumFollow, numIterations, updateVelocity, psoExampleScorer)
        .execute()

      println(best)
      best
    }
  }

  def psoExampleScorer(particle: TraitSeq[Double]): Double = {
    //val ts = traitsequence.asInstanceOf[TraitSeqVal[Double]]
    -1.0
  }

  def updateVelocity(particle: Particle[Double],
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
      newVel(i) = v + globalOptimumFollow * globalBest.position(i) + localOptimumFollow * lb(i)
    })

    val newPos = particle.position.deepcopy()
    (0 until particle.position.length)
      .foreach(i => {
      newPos(i) = particle.position(i) + newVel(i)
    })

    val oldscore = scorer(particle.position)
    val newscore = scorer(newPos)

    new Particle[Double](newPos, newVel, if (newscore > oldscore)
                                           newPos
                                         else
                                           particle.position)
  }

}

