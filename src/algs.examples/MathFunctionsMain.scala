package com.derek.algs.examples

import algs.meta_heuristics.util.{Scoring, Output, TimedExecution}
import scala.util.Random
import algs.meta_heuristics.structures.concrete.infinite.neighbourhood.DoubleTraitSeqVal
import algs.meta_heuristics.structures.specification.TraitSeq
import algs.meta_heuristics.{Tabusearch, ParticleSwarm, GeneticAlgorithm}
import algs.meta_heuristics.particle.Particle

/**
 * @author Derek Hawker
 */
object MathFunctionsMain
{
  val featureUpperBound      = 600851475000000000000.0
  val numFeatures            = 25
  val featureNumericalBounds = Array.range(0, numFeatures)
    .map(tr =>
  {
    val featureUpper: Double = featureUpperBound / 2
    val featureLower: Double = -featureUpperBound / 2
    (featureUpper, featureLower)
  })

  def main(args: Array[String])
  {


    //gaTest


    //tabuTest


    psoTest()
  }

  def psoTest(): Unit =
  {
    new TimedExecution().execute
    {

      val velocityFollow = 1.4
      val localOptimumFollow = 0.5
      val globalOptimumFollow = 0.6
      val numIterations = 4000
      val numPopulation = 20

      val population = Array.range(0, numPopulation)
        .map(p =>
      {
        val initWeights = Array.range(0, numFeatures)
          .map(m => Random.nextDouble() * featureUpperBound - featureUpperBound / 2.0)

        val initVelocity = Array.range(0, numFeatures)
          .map(m => Random.nextDouble() * 10000000L - 1000000L / 2)

        new Particle[Double](new DoubleTraitSeqVal(initWeights, featureNumericalBounds),
          new DoubleTraitSeqVal(initVelocity, featureNumericalBounds),
          new DoubleTraitSeqVal(initWeights, featureNumericalBounds),
          null)
      })

      val positionBounds = Array.range(0, numFeatures)
        .map(m => (-featureUpperBound / 2.0, featureUpperBound / 2.0))

      val best = ParticleSwarm.defaultArguments[Double](population, positionBounds,
        Particle.updateVelocity, Particle.updatePosition, Scoring.griewank)
        .execute()

      println(best)
      best
    }
  }

  private def tabuTest
  {
    new TimedExecution().execute
    {
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

  private def gaTest
  {
    new TimedExecution().execute
    {
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
                                          scores: Array[Double]): Boolean =
  {
    true
  }

  private def endOfIterationCondition[T](iteration: Int,
                                         globalBest: TraitSeq[T],
                                         globalBestScore: Double,
                                         localBest: TraitSeq[T],
                                         localBestScore: Double): Boolean =
  {
    true
  }
}











