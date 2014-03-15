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
        globalOptimumFollow, localOptimumFollow, numIterations, updateVelocity, iterationPrinter,
        griewank)
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


  /**
   *
   * @param traitsequence
   * @return
   */
  def griewank(traitsequence: TraitSeq[Double]): Double =
    -(1 +
      (traitsequence.foldLeft(0.0)(
        (count, d) =>
          count + math.pow(d, 2))
        / 4000)
      - traitsequence.zipWithIndex
      .foldLeft(1.0)(
        (count, pair) => {
          val i = pair._2
          count * math.cos(pair._1 / math.sqrt(i + 1))
        }
      ))


  def iterationPrinter[T](i: Int,
                          population: Array[Particle[T]],
                          globalBest: Particle[T],
                          globalBestScore: Double) {
    println("^iteration: " + i)
    println("\t[Global]Best: (%f) %s".format(globalBestScore, globalBest.position))

    population
      .foreach(p => {
      println("\t" + p.position + " " + p.velocity + "score: " + griewank(
        p.position.asInstanceOf[TraitSeqVal[Double]]))
    })
  }
}











