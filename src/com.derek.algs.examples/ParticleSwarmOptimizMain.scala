package com.derek.algs.examples

import com.derek.algs.ParticleSwarmOptimization
import com.derek.algs.util.{Output, Scoring, TimedExecution}
import scala.util.Random
import com.derek.algs.structures.specification.TraitSeq
import com.derek.algs.particle.swarm.optimization.Particle
import com.derek.algs.structures.concrete.finite.neighbourhood.TraitSeqVal

/**
 * @author Derek Hawker
 */
object ParticleSwarmOptimizMain {
  def main(args: Array[String]) {

    val velocityFollow = 0.4
    val localOptimumFollow = 0.1
    val globalOptimumFollow = 0.2
    val numIterations = 400
    val numPopulation = 5
    val numFeatures = 1

    val population = Array.range(0, numPopulation)
      .map(p => {
      val initWeights = Array.range(0, numFeatures)
        .map(m => Random.nextDouble() * 400 - 200)

      val initVelocity = Array.range(0, numFeatures)
        .map(m => Random.nextDouble() * 400 - 200)

      new Particle[Double](new TraitSeqVal[Double](initWeights, null),
        new TraitSeqVal[Double](initVelocity, null),
        new TraitSeqVal[Double](initWeights, null))
    })

    new TimedExecution().execute {
      val best = new ParticleSwarmOptimization[Double](population, velocityFollow,
        globalOptimumFollow, localOptimumFollow, numIterations, Particle.updateVelocityDouble,
        Output.psoIterationPrinter,
        Scoring.griewank)
        .execute()

      println(best)
      best
    }
  }
}











