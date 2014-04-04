package com.derek.algs.examples

import com.derek.algs.{GeneticAlgorithm, Tabusearch, ParticleSwarm}
import com.derek.algs.util.{Scoring, Output, TimedExecution}
import scala.util.Random
import com.derek.algs.particle.swarm.optimization.Particle
import com.derek.algs.structures.concrete.infinite.neighbourhood.DoubleTraitSeqVal
import com.derek.algs.structures.specification.TraitSeq

/**
 * @author Derek Hawker
 */
object MathFunctionsMain {
  val featureUpperBound      = 600851475000000000000.0
  val numFeatures            = 50
  val featureNumericalBounds = Array.range(0, numFeatures)
    .map(tr => {
    val featureUpper: Double = featureUpperBound / 2
    val featureLower: Double = -featureUpperBound / 2
    (featureUpper, featureLower)
  })

  def main(args: Array[String]) {


    //gaTest


    //tabuTest


    psoTest
  }

  def psoTest {
    new TimedExecution().execute {

      val velocityFollow = 0.50
      val localOptimumFollow = 0.40
      val globalOptimumFollow = 0.73
      val numIterations = 400
      val numPopulation = 500
      val numGaGenerations = 300
      val numGaPopulation = 5000
      val mutationRate = 0.4
      val tabuTimeToLive = 5

      val population = Array.range(0, numPopulation)
        .map(p => {
        val initWeights = Array.range(0, numFeatures)
          .map(m => Random.nextDouble() * featureUpperBound - featureUpperBound / 2.0)

        val initVelocity = Array.range(0, numFeatures)
          .map(m => Random.nextDouble() * 10000000L - 1000000L / 2)

        new Particle[Double](new DoubleTraitSeqVal(initWeights, featureNumericalBounds),
          new DoubleTraitSeqVal(initVelocity, featureNumericalBounds),
          new DoubleTraitSeqVal(initWeights, featureNumericalBounds))
      })

      val positionBounds = Array.range(0, numFeatures)
        .map(m => (-featureUpperBound / 2.0, featureUpperBound / 2.0))

      val best = new ParticleSwarm[Double](population, positionBounds, velocityFollow,
        globalOptimumFollow, localOptimumFollow, numIterations, Particle.updateVelocity,
      Particle.updatePosition,
        endOfPsoIterationCondition, Output.psoIterationPrinter, Scoring.griewank)
        .execute()

      println(best)
      best
    }
  }

  private def tabuTest {
    new TimedExecution().execute {
      val numIterations = 400
      val tabuTimeToLive = 5

      val startingSolution =
        new DoubleTraitSeqVal(Array.range(0, numFeatures)
          .map(tr =>
          Random.nextDouble() * featureUpperBound - featureUpperBound / 2.0),
          featureNumericalBounds).asInstanceOf[TraitSeq[Double]]


      val best = new Tabusearch[Double](startingSolution, tabuTimeToLive, numIterations,
        endOfIterationCondition, Output.defaultIterationPrinter, Scoring.griewank)
        .execute()

      println(best)
      best
    }
  }

  private def gaTest {
    new TimedExecution().execute {
      val numGenerations = 300
      val numPopulation = 5000
      val mutationRate = 0.4


      val population = Array.range(0, numPopulation)
        .map(person =>
        new DoubleTraitSeqVal(Array.range(0, numFeatures)
          .map(tr =>
          Random.nextDouble() * featureUpperBound - featureUpperBound / 2.0),
          featureNumericalBounds).asInstanceOf[TraitSeq[Double]])


      val best = new GeneticAlgorithm[Double](population, numGenerations, mutationRate,
        endOfGenerationCondition, GeneticAlgorithm.Mating.eliteSelection,
        GeneticAlgorithm.BabyMaker.spliceTwoParents, Output.defaultIterationPrinter,
        Scoring.griewank)
        .execute()

      println(best)

      best
    }
  }

  private def endOfGenerationCondition[T](generation: Int,
                                          population: Array[TraitSeq[T]],
                                          scores: Array[Double]): Boolean = {
    true
  }

  private def endOfIterationCondition[T](iteration: Int,
                                         globalBest: TraitSeq[T],
                                         globalBestScore: Double,
                                         localBest: TraitSeq[T],
                                         localBestScore: Double): Boolean = {
    true
  }

  def endOfPsoIterationCondition[T](iteration: Int,
                                    population: Array[Particle[T]],
                                    globalBest: Particle[T],
                                    globalBestScore: Double,
                                    localBest: Particle[T],
                                    localBestScore: Double): Boolean = {
    true
  }

}











