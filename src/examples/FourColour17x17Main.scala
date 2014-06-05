package examples

import java.io.{FileReader, BufferedReader}
import weka.core.Instances
import logging.Logger
import meta_heuristics.util.{Scoring, Output, TimedExecution}
import meta_heuristics.particle_swarm_optimization.particle.Particle
import meta_heuristics.structures.concrete.infinite.neighbourhood.{INTraitSeqVal, DoubleTraitSeqVal}
import meta_heuristics.{Tabusearch, GeneticAlgorithm, ParticleSwarm}
import scala.util.Random
import meta_heuristics.structures.specification.TraitSeq
import meta_heuristics.util.gif.AnimatedProgressGif
import meta_heuristics.structures.concrete.finite.neighbourhood.{PackedSeqVal, TraitSeqVal}

/**
 * @author Derek Hawker
 */
object FourColour17x17Main
{

  val logger = Logger(FourColour17x17Main.getClass.toString)

  def main(args: Array[String])
  {

    //        gaTest()
    packedGaTest()
    //
    //
    //        tabuTest

    //      packedTabuTest

    //    psoTest

  }


  def psoTest
  {
    new TimedExecution().execute
    {
      val velocityFollow = 1
      val localOptimumFollow = 0.9
      val globalOptimumFollow = 0.9
      val numFeatures: Int = 17 * 17
      val numIterations = 100
      val numPopulation = 50


      val population = Array.range(0, numPopulation)
        .map(person =>
      {

        val startPos: INTraitSeqVal[Double] = new INTraitSeqVal(
          Array.range(0, numFeatures)
            .map(tr =>
            Random.nextDouble() * 3.0))

        new Particle[Double](
          startPos.asInstanceOf[TraitSeq[Double]],

          new INTraitSeqVal(
            Array.range(0, numFeatures)
              .map(tr =>
              Random.nextDouble() * 0.50 - 0.25)).asInstanceOf[TraitSeq[Double]],
          startPos,
          null)

      })

      def printer[T](i: Int,
                     population: Array[Particle[T]],
                     scores: Array[Double],
                     globalBest: Particle[T],
                     globalBestScore: Double,
                     localBest: Particle[T],
                     localBestScore: Double): Unit =
      {
        Output.psoIterationPrinter(i, population, scores, globalBest, globalBestScore, localBest,
          localBestScore)
        //        AnimatedProgressGif.apply.addFrame(globalBest.position.asInstanceOf[TraitSeq[Int]])
      }

      val positionBounds = Array.range(0, numFeatures).map(m => (0.0, 3.0))


      AnimatedProgressGif("visualizations/pso.gif")
      val best = new ParticleSwarm[Double](population, positionBounds, velocityFollow,
        globalOptimumFollow, localOptimumFollow, numIterations, Particle.updateVelocity,
        Particle.updatePosition,
        endOfPsoIterationCondition, printer, Scoring.doubleColour17x17Scorer)
        .execute()

      logger.info(best.toString)
      AnimatedProgressGif.apply.addFrame(best.asInstanceOf[TraitSeq[Int]])
      AnimatedProgressGif.apply.finish()

      best
    }
  }

  private def tabuTest()
  {
    new TimedExecution().execute
    {
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
                     localBestScore: Double)
      {
        Output
          .defaultIterationPrinter(i, population, scores, globalBest, globalBestScore, localBest,
            localBestScore)
        AnimatedProgressGif.apply.addFrame(globalBest.asInstanceOf[TraitSeq[Int]])
      }


      AnimatedProgressGif("visualizations/tabu.gif")
      val best = new Tabusearch[Int](startingSolution, tabuTimeToLive, numIterations,
        endOfIterationCondition, printer, Scoring.fourColour17x17Scorer)
        .execute()


      logger.info(best.toString)
      AnimatedProgressGif.apply.addFrame(best.asInstanceOf[TraitSeq[Int]])
      AnimatedProgressGif.apply.finish()

      best
    }
  }

  private def packedTabuTest()
  {
    new TimedExecution().execute
    {
      val numFeatures: Int = 17 * 17
      val numIterations = 400
      val tabuTimeToLive = 5

      val neighbourhood = Array.range(0, numFeatures)
        .map(i =>
        Array.range(0, 4))

      val startingSolution =
        new PackedSeqVal(2, numFeatures, Array.range(0, numFeatures / 16 + 1)
          .map(tr =>
          Random.nextInt(Int.MaxValue)),
          neighbourhood).asInstanceOf[TraitSeq[Int]]

      def printer[T](i: Int,
                     population: Array[TraitSeq[T]],
                     scores: Array[Double],
                     globalBest: TraitSeq[T],
                     globalBestScore: Double,
                     localBest: TraitSeq[T],
                     localBestScore: Double)
      {
        Output
          .defaultIterationPrinter(i, population, scores, globalBest, globalBestScore, localBest,
            localBestScore)
        AnimatedProgressGif.apply.addFrame(globalBest.asInstanceOf[TraitSeq[Int]])
      }


      AnimatedProgressGif("visualizations/tabu.gif")
      val best = new Tabusearch[Int](startingSolution, tabuTimeToLive, numIterations,
        endOfIterationCondition, printer, Scoring.fourColour17x17Scorer)
        .execute()


      logger.info(best.toString)
      AnimatedProgressGif.apply.addFrame(best.asInstanceOf[TraitSeq[Int]])
      AnimatedProgressGif.apply.finish()

      best
    }
  }


  private def gaTest()
  {
    new TimedExecution().execute
    {
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
                     localBestScore: Double)
      {
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

      logger.info(best.toString)
      AnimatedProgressGif.apply.addFrame(best.asInstanceOf[TraitSeq[Int]])
      AnimatedProgressGif.apply.finish()

      best
    }
  }

  private def packedGaTest()
  {
    new TimedExecution().execute
    {
      val numFeatures: Int = 17 * 17
      val numGenerations = 200
      val numPopulation = 52
      val mutationRate = 0.4


      val neighbourhood = Array.range(0, numFeatures)
        .map(i =>
        Array.range(0, 4))

      val population = Array.range(0, numPopulation)
        .map(person =>
        new PackedSeqVal(2, numFeatures, Array.range(0, numFeatures / 16 + 1)
          .map(tr =>
          Random.nextInt(Int.MaxValue)),
          neighbourhood).asInstanceOf[TraitSeq[Int]])

      def printer[T](generation: Int,
                     population: Array[TraitSeq[T]],
                     scores: Array[Double],
                     globalBest: TraitSeq[T],
                     globalBestScore: Double,
                     localBest: TraitSeq[T],
                     localBestScore: Double)
      {
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

      logger.info(best.toString)
      AnimatedProgressGif.apply.addFrame(best.asInstanceOf[TraitSeq[Int]])
      AnimatedProgressGif.apply.finish()

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

  def endOfPsoIterationCondition[T](iteration: Int,
                                    population: Array[Particle[T]],
                                    globalBest: Particle[T],
                                    globalBestScore: Double,
                                    localBest: Particle[T],
                                    localBestScore: Double): Boolean =
  {
    true
  }
}
