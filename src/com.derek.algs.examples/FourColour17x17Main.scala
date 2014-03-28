package com.derek.algs.examples

import scala.util.Random
import com.derek.algs.util.{Scoring, Output, TimedExecution}
import com.derek.algs.{Tabusearch, GeneticAlgorithm, ParticleSwarmOptimization}
import com.derek.algs.structures.specification.TraitSeq
import com.derek.algs.particle.swarm.optimization.Particle
import com.derek.algs.util.gif.AnimatedProgressGif
import com.derek.algs.structures.concrete.finite.neighbourhood.TraitSeqVal

/**
 * @author Derek Hawker
 */
object FourColour17x17Main {
  def main(args: Array[String]) {

    gaTest


    tabuTest


    psoTest

  }


  def psoTest {
    new TimedExecution().execute {
      val velocityFollow = 1
      val localOptimumFollow = 1.3
      val globalOptimumFollow = 0.7
      val numFeatures: Int = 17 * 17
      val numIterations = 100
      val numPopulation = 50

      val neighbourhood = Array.range(0, numFeatures)
        .map(i =>
        Array.range(0, 4)
        )

      val population = Array.range(0, numPopulation)
        .map(person => {

        val startPos: TraitSeqVal[Int] = new TraitSeqVal(
          Array.range(0, numFeatures)
            .map(tr =>
            Random.nextInt(4)),
          neighbourhood)

        new Particle[Int](
          startPos.asInstanceOf[TraitSeq[Int]],
          new TraitSeqVal(
            Array.range(0, numFeatures)
              .map(tr =>
              Random.nextInt(2) - 1),
            neighbourhood).asInstanceOf[TraitSeq[Int]],
          startPos)

      })

      def printer[T](i: Int,
                     population: Array[Particle[T]],
                     scores: Array[Double],
                     globalBest: Particle[T],
                     globalBestScore: Double,
                     localBest: Particle[T],
                     localBestScore: Double): Unit = {
        Output.psoIterationPrinter(i, population, scores, globalBest, globalBestScore, localBest,
          localBestScore)
        AnimatedProgressGif.apply.addFrame(globalBest.position.asInstanceOf[TraitSeq[Int]])
      }

      AnimatedProgressGif("visualizations/pso.gif")
      val best = new ParticleSwarmOptimization[Int](population, velocityFollow, globalOptimumFollow,
        localOptimumFollow, numIterations, Particle.updateVelocityInt,
        endOfPsoIterationCondition, printer, Scoring.fourColour17x17Scorer)
        .execute()

      println(best)
      AnimatedProgressGif.apply.addFrame(best.asInstanceOf[TraitSeq[Int]])
      AnimatedProgressGif.apply.finish()

      best
    }
  }

  private def tabuTest {
    new TimedExecution().execute {
      val numFeatures: Int = 17 * 17
      val numIterations = 400
      val tabuTimeToLive = 5

      val neighbourhood = Array.range(0, numFeatures)
        .map(i =>
        Array.range(0, 4))

      val startingSolution =
        new TraitSeqVal(Array.range(0, numFeatures)
          .map(tr =>
          Random.nextInt(4)),
          neighbourhood).asInstanceOf[TraitSeq[Int]]

      def printer[T](i: Int,
                     population: Array[TraitSeq[T]],
                     scores: Array[Double],
                     globalBest: TraitSeq[T],
                     globalBestScore: Double,
                     localBest: TraitSeq[T],
                     localBestScore: Double) {
        Output.defaultIterationPrinter(i, population, scores, globalBest, globalBestScore, localBest, localBestScore)
        AnimatedProgressGif.apply.addFrame(globalBest.asInstanceOf[TraitSeq[Int]])
      }


      AnimatedProgressGif("visualizations/tabu.gif")
      val best = new Tabusearch[Int](startingSolution, tabuTimeToLive, numIterations,
        endOfIterationCondition, printer, Scoring.fourColour17x17Scorer)
        .execute()


      println(best)
      AnimatedProgressGif.apply.addFrame(best.asInstanceOf[TraitSeq[Int]])
      AnimatedProgressGif.apply.finish()

      best
    }
  }

  private def gaTest {
    new TimedExecution().execute {
      val numFeatures: Int = 17 * 17
      val numGenerations = 200
      val numPopulation = 52
      val mutationRate = 0.4


      val neighbourhood = Array.range(0, numFeatures)
        .map(i =>
        Array.range(0, 4))

      val population = Array.range(0, numPopulation)
        .map(person =>
        new TraitSeqVal(Array.range(0, numFeatures)
          .map(tr =>
          Random.nextInt(4)),
          neighbourhood).asInstanceOf[TraitSeq[Int]])

      def printer[T](generation: Int,
                     population: Array[TraitSeq[T]],
                     scores: Array[Double],
                     globalBest: TraitSeq[T],
                     globalBestScore: Double,
                     localBest: TraitSeq[T],
                     localBestScore: Double) {
        Output.defaultIterationPrinter(generation, population, scores, globalBest, globalBestScore,
          localBest, localBestScore)
        AnimatedProgressGif.apply.addFrame(globalBest.asInstanceOf[TraitSeq[Int]])
      }

      AnimatedProgressGif("visualizations/ga.gif")
      val best = new GeneticAlgorithm[Int](population, numGenerations, mutationRate,
        endOfGenerationCondition, GeneticAlgorithm.Mating.eliteSelection,
        GeneticAlgorithm.BabyMaker.spliceTwoParents, printer,
        Scoring.fourColour17x17Scorer)
        .execute()

      println(best)
      AnimatedProgressGif.apply.addFrame(best.asInstanceOf[TraitSeq[Int]])
      AnimatedProgressGif.apply.finish()

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
