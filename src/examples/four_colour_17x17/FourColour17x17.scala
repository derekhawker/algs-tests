package examples.four_colour_17x17

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
import meta_heuristics.output.{DefaultBranchAndBoundIterationOutput, DefaultPSOIterationOutput}
import optimization.BranchAndBound
import optimization.BranchAndBound.Solution
import meta_heuristics.genetic_algorithms.RandomMutation

/**
 * @author Derek Hawker
 */
object FourColour17x17
{

   val logger = Logger(FourColour17x17.getClass.toString)

   val velocityFollow      = 1
   val localOptimumFollow  = 0.9
   val globalOptimumFollow = 0.9
   val numFeatures         = 17 * 17
   val numPSIterations     = 100
   val numPSOPopulation    = 50

   val numIterations  = 400
   val tabuTimeToLive = 5

   val numGApopulation  = 52
   val numGAgenerations = 200
   val mutationRate     = 0.4

   val neighbourhood = Array.range(0, numFeatures)
      .map(i =>
      Array.range(0, 4))


   def main(args: Array[String])
   {
      //      gaTest()
      //      packedGaTest()
      //      tabuTest()
      //      packedTabuTest()
      //      psoTest()
      branchAndBoundTest()
   }


   private def psoTest()
   {

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

      val pso = new ParticleSwarm[Double](population, positionBounds,
         velocityFollow, globalOptimumFollow, localOptimumFollow, numIterations)
         with DoubleVelocityUpdate with DoublePositionUpdate with IgnoredPSOCondition[Double]
         with DefaultPSOIterationOutput[Double] with DoubleScorer
      {
         override def printIteration(iteration: Int, population: Array[Particle[Double]],
                                     scores: Array[Double], globalBest: Particle[Double],
                                     globalBestScore: Double, localBest: Particle[Double],
                                     localBestScore: Double): Unit =
         {

            super.printIteration(iteration, population, scores, globalBest, globalBestScore,
               localBest, localBestScore)
            //        AnimatedProgressGif.apply.addFrame(globalBest.position.asInstanceOf[TraitSeq[Int]])
         }
      }

      AnimatedProgressGif("visualizations/pso.gif")

      val best = new TimedExecution().execute {
         pso.execute()
      }

      logger.info(best.toString)
      AnimatedProgressGif.apply.addFrame(pso.asInstanceOf[TraitSeq[Int]])
      AnimatedProgressGif.apply.finish()
   }

   private def tabuTest()
   {

      val startingSolution =
         new TraitSeqVal(Array.range(0, numFeatures)
            .map(tr =>
            Random.nextInt(4)),
            neighbourhood).asInstanceOf[TraitSeq[Int]]

      _tabuTest(startingSolution)
   }

   private def packedTabuTest()
   {

      val startingSolution =
         new PackedSeqVal(2, numFeatures, Array.range(0, numFeatures / 16 + 1)
            .map(tr =>
            Random.nextInt(Int.MaxValue)),
            neighbourhood).asInstanceOf[TraitSeq[Int]]

      _tabuTest(startingSolution)

   }

   private def _tabuTest(startingSolution: TraitSeq[Int])
   {
      val tbs = new Tabusearch[Int](startingSolution, tabuTimeToLive, numIterations)
         with IgnoredIterationConditionCheck[Int] with Printer
         with IntScorer

      AnimatedProgressGif("visualizations/tabu.gif")

      val best = new TimedExecution().execute {
         tbs.execute()
      }

      logger.info(best.toString)
      AnimatedProgressGif.apply.addFrame(
         best.asInstanceOf[TraitSeq[Int]])
      AnimatedProgressGif.apply.finish()

   }


   private def gaTest()
   {
      val population = Array.range(0, FourColour17x17.numGApopulation)
         .map(person =>
         new TraitSeqVal(Array.range(0, numFeatures)
            .map(tr =>
            Random.nextInt(4)),
            neighbourhood).asInstanceOf[TraitSeq[Int]])

      _gaTest(population)
   }

   private def packedGaTest()
   {
      val population = Array.range(0, FourColour17x17.numGApopulation)
         .map(person =>
         new PackedSeqVal(2, numFeatures, Array.range(0, numFeatures / 16 + 1)
            .map(tr =>
            Random.nextInt(Int.MaxValue)),
            neighbourhood).asInstanceOf[TraitSeq[Int]])

      _gaTest(population)
   }

   private def _gaTest(population: Array[TraitSeq[Int]])
   {
      val ga = new GeneticAlgorithm[Int](population, FourColour17x17.numGAgenerations,
         FourColour17x17.mutationRate)
         with IgnoredIterationConditionCheck[Int] with EliteSelection[Int]
         with SpliceParents[Int] with Printer with IntScorer
         with RandomMutation[Int]

      AnimatedProgressGif("visualizations/ga.gif")

      val best = new TimedExecution().execute {
         ga.execute()
      }

      logger.info(best.toString)

      AnimatedProgressGif.apply.addFrame(
         best.asInstanceOf[TraitSeq[Int]])
      AnimatedProgressGif.apply.finish()
   }

   private def branchAndBoundTest()
   {
      val incumbentPattern = "3332112030120011003112122223230013132213020003113201230132311003022200303230113212311030030022321111323220331012221310330011031102230232203301210111230321122210003332023121030312313122210001332112203312013202131323100200121102031002312023012102233010123331201023033012312123120203121103302"
         .map(_.toInt - 48).toArray
      val incumbentSolution = Some(new TraitSeqVal[Int](incumbentPattern, neighbourhood))


      val pattern = Array.range(4, numFeatures + 4)
         .map(i => i)

      //      val pattern = Array.range(0, numFeatures)
      //         .map(i =>
      //               Random.nextInt(4))

      //      val pattern = Array.range(0, numFeatures)
      //         .map(i =>
      ////         (((i % 17) % 4) + ((i / 17) % 4)) % 4)

      for (i <- 0 until 17) {
         for (j <- 0 until 17) {
            print(pattern(i * 17 + j))
         }
         println()
      }


      val patternSolution = new TraitSeqVal[Int](pattern, neighbourhood)
      println(patternSolution)

      val bestFirstOrdering = new Ordering[Solution[Int]]
      {
         override def compare(x: Solution[Int], y: Solution[Int]): Int =
            (x._2 - y._2).toInt
      }

      val depthFirstOrdering = new Ordering[Solution[Int]]
      {
         override def compare(x: Solution[Int], y: Solution[Int]): Int =
            x._1.level - y._1.level
      }

      val depthFirstSolutionScoreOrdering = new Ordering[Solution[Int]]
      {
         override def compare(x: Solution[Int], y: Solution[Int]): Int =
            if (x._1.level == y._1.level)
               (x._2 - y._2).toInt
            else
               x._1.level - y._1.level
      }

      val breadthFirstOrdering = new Ordering[Solution[Int]]
      {
         override def compare(x: Solution[Int], y: Solution[Int]): Int =
            (y._1.level - x._1.level)
      }

      val solutionWeightedOrdering = new Ordering[Solution[Int]]
      {
         override def compare(x: Solution[Int], y: Solution[Int]): Int =
            if (x._2 == y._2)
               (y._1.level - x._1.level)
            else
               (x._2 - y._2).toInt
      }
      val bnb = new BranchAndBound[Int](patternSolution, depthFirstOrdering, incumbentSolution,
         neighbourhood)
         with IntScorer with DefaultBranchAndBoundIterationOutput[Int]
         with BranchAndBoundFeasibleSolution with BranchAndBoundGetIndex

      AnimatedProgressGif("visualizations/ga.gif")

      val best = new TimedExecution().execute {
         bnb.execute()
      }

      logger.info(best.toString)

      AnimatedProgressGif.apply.addFrame(best.asInstanceOf[TraitSeq[Int]])
      AnimatedProgressGif.apply.finish()
   }
}

