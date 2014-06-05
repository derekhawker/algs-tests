package examples

import logging.Logger
import meta_heuristics.util.TimedExecution
import meta_heuristics.particle_swarm_optimization.particle.{DoubleVelocityUpdate, DoublePositionUpdate, Particle}
import meta_heuristics.structures.concrete.infinite.neighbourhood.INTraitSeqVal
import meta_heuristics._
import scala.util.Random
import meta_heuristics.structures.specification.TraitSeq
import meta_heuristics.util.gif.AnimatedProgressGif
import meta_heuristics.structures.concrete.finite.neighbourhood.{PackedSeqVal, TraitSeqVal}
import meta_heuristics.genetic_algorithms.population_selector.EliteSelection
import meta_heuristics.genetic_algorithms.babies.SpliceParents
import meta_heuristics.output.{DefaultPSOIterationOutput, FourColour17x17Printer}
import meta_heuristics.scoring.{FourColour17x17Scorer, FourColour17x17DoubleScorer}

/**
 * @author Derek Hawker
 */
object FourColour17x17
{

  val logger = Logger(FourColour17x17.getClass.toString)

  val velocityFollow      = 1
  val localOptimumFollow  = 0.9
  val globalOptimumFollow = 0.9
  val numFeatures: Int    = 17 * 17
  val numPSIterations     = 100
  val numPSOPopulation    = 50

  val numIterations  = 400
  val tabuTimeToLive = 5

  val numGApopulation  = 50
  val numGAgenerations = 200
  val mutationRate     = 0.4

  def main(args: Array[String])
  {

    gaTest()
    //    packedGaTest()
    //
    //
    //        tabuTest

    //      packedTabuTest

    //    psoTest

  }


  def psoTest
  {
    new TimedExecution().execute {

      val population = Array.range(0, FourColour17x17.numPSOPopulation)
        .map(person => {

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


      val positionBounds = Array.range(0, numFeatures).map(m => (0.0, 3.0))


      AnimatedProgressGif("visualizations/pso.gif")
      val best = new ParticleSwarm[Double](population, positionBounds, velocityFollow,
        globalOptimumFollow, localOptimumFollow, numIterations) with DoubleVelocityUpdate
        with DoublePositionUpdate with IgnoredPSOCondition[Double]
        with DefaultPSOIterationOutput[Double] with FourColour17x17DoubleScorer
      {
        override def printIteration(iteration: Int, population: Array[Particle[Double]],
                                    scores: Array[Double], globalBest: Particle[Double],
                                    globalBestScore: Double, localBest: Particle[Double],
                                    localBestScore: Double): Unit =
        {

          super
            .printIteration(iteration, population, scores, globalBest, globalBestScore, localBest,
              localBestScore)
          //        AnimatedProgressGif.apply.addFrame(globalBest.position.asInstanceOf[TraitSeq[Int]])
        }
      }
        .execute()

      logger.info(best.toString)
      AnimatedProgressGif.apply.addFrame(best.asInstanceOf[TraitSeq[Int]])
      AnimatedProgressGif.apply.finish()

      best
    }
  }

  private def tabuTest()
  {
    new TimedExecution().execute {

      val neighbourhood = Array.range(0, numFeatures)
        .map(i =>
        Array.range(0, 4))

      val startingSolution =
        new TraitSeqVal(Array.range(0, numFeatures)
          .map(tr =>
          Random.nextInt(4)),
          neighbourhood).asInstanceOf[TraitSeq[Int]]


      AnimatedProgressGif("visualizations/tabu.gif")
      val tbs = new Tabusearch[Int](startingSolution, tabuTimeToLive, numIterations)
        with IgnoredGeneticAlgorithmCondition[Int] with FourColour17x17Printer
        with FourColour17x17Scorer
      val best = tbs.execute()


      logger.info(best.toString)
      AnimatedProgressGif.apply.addFrame(best.asInstanceOf[TraitSeq[Int]])
      AnimatedProgressGif.apply.finish()

      best
    }
  }


  private def packedTabuTest()
  {
    new TimedExecution().execute {
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


      AnimatedProgressGif("visualizations/tabu.gif")
      val tbs = new Tabusearch[Int](startingSolution, tabuTimeToLive, numIterations)
        with IgnoredGeneticAlgorithmCondition[Int] with FourColour17x17Printer
        with FourColour17x17Scorer
      val best = tbs.execute()


      logger.info(best.toString)
      AnimatedProgressGif.apply.addFrame(best.asInstanceOf[TraitSeq[Int]])
      AnimatedProgressGif.apply.finish()

      best
    }
  }


  private def gaTest()
  {
    new TimedExecution().execute {

      val neighbourhood = Array.range(0, numFeatures)
        .map(i =>
        Array.range(0, 4))

      val population = Array.range(0, FourColour17x17.numGApopulation)
        .map(person =>
        new TraitSeqVal(Array.range(0, numFeatures)
          .map(tr =>
          Random.nextInt(4)),
          neighbourhood).asInstanceOf[TraitSeq[Int]])


      AnimatedProgressGif("visualizations/ga.gif")
      val ga = new GeneticAlgorithm[Int](population, FourColour17x17.numGAgenerations,
        FourColour17x17.mutationRate)
        with IgnoredGeneticAlgorithmCondition[Int] with EliteSelection[Int]
        with SpliceParents[Int] with FourColour17x17Printer with FourColour17x17Scorer

      val best = ga.execute()

      logger.info(best.toString)
      AnimatedProgressGif.apply.addFrame(best.asInstanceOf[TraitSeq[Int]])
      AnimatedProgressGif.apply.finish()

      best
    }
  }

  private def packedGaTest()
  {
    new TimedExecution().execute {

      val neighbourhood = Array.range(0, numFeatures)
        .map(i =>
        Array.range(0, 4))

      val population = Array.range(0, FourColour17x17.numGApopulation)
        .map(person =>
        new PackedSeqVal(2, numFeatures, Array.range(0, numFeatures / 16 + 1)
          .map(tr =>
          Random.nextInt(Int.MaxValue)),
          neighbourhood).asInstanceOf[TraitSeq[Int]])

      AnimatedProgressGif("visualizations/ga.gif")
      val ga = new GeneticAlgorithm[Int](population, FourColour17x17.numGAgenerations,
        FourColour17x17.mutationRate)
        with IgnoredGeneticAlgorithmCondition[Int] with EliteSelection[Int]
        with SpliceParents[Int] with FourColour17x17Printer with FourColour17x17Scorer

      val best = ga.execute()

      logger.info(best.toString)

      AnimatedProgressGif.apply.addFrame(best.asInstanceOf[TraitSeq[Int]])
      AnimatedProgressGif.apply.finish()

      best
    }
  }
}
