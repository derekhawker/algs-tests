package examples

import meta_heuristics.util.TimedExecution
import meta_heuristics.particle_swarm_optimization.particle.{DoublePositionUpdate, DoubleVelocityUpdate, Particle}
import meta_heuristics.structures.concrete.infinite.neighbourhood.DoubleTraitSeqVal
import meta_heuristics._
import scala.util.Random
import meta_heuristics.structures.specification.TraitSeq
import meta_heuristics.output.{DefaultIterationOutput, DefaultPSOIterationOutput}
import meta_heuristics.scoring.GriewankDouble
import meta_heuristics.genetic_algorithms.population_selector.EliteSelection
import meta_heuristics.genetic_algorithms.babies.SpliceParents

/**
 * @author Derek Hawker
 */
object MathFunctionsMain
{
   val featureUpperBound      = 600851475000000000000.0
   val numFeatures            = 25
   val featureNumericalBounds = Array.range(0, numFeatures)
      .map(tr => {
      val featureUpper: Double = featureUpperBound / 2
      val featureLower: Double = -featureUpperBound / 2
      (featureUpper, featureLower)
   })

   def main(args: Array[String])
   {
      //    gaTest

      //    tabuTest
      //
      psoTest()
   }

   def psoTest(): Unit =
   {
      new TimedExecution().execute {

         val velocityFollow = 1.4
         val localOptimumFollow = 0.5
         val globalOptimumFollow = 0.6
         val numIterations = 4000
         val numPopulation = 20

         val population = Array.range(0, numPopulation)
            .map(p => {
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

         val pso = new ParticleSwarm[Double](population, positionBounds,
            ParticleSwarm.velocityFollow,
            ParticleSwarm.globalOptimumFollow, ParticleSwarm.localOptimumFollow,
            ParticleSwarm.numIterations) with DoubleVelocityUpdate with DoublePositionUpdate
            with IgnoredPSOCondition[Double] with DefaultPSOIterationOutput[Double] with GriewankDouble

         val best = pso.execute()

         println(best)
         best
      }
   }

   private def tabuTest
   {
      new TimedExecution().execute {
         val numIterations = 400
         val tabuTimeToLive = 5

         val startingSolution =
            new DoubleTraitSeqVal(Array.range(0, numFeatures)
               .map(tr =>
               Random.nextDouble() * featureUpperBound - featureUpperBound / 2.0),
               featureNumericalBounds).asInstanceOf[TraitSeq[Double]]


         val tbs = new Tabusearch[Double](startingSolution, tabuTimeToLive, numIterations)
            with IgnoredGeneticAlgorithmCondition[Double] with DefaultIterationOutput[Double]
            with GriewankDouble

         val best = tbs.execute()

         println(best)
         best
      }
   }

   private def gaTest
   {
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

         val ga = new GeneticAlgorithm[Double](population, numGenerations, mutationRate)
            with IgnoredGeneticAlgorithmCondition[Double] with EliteSelection[Double]
            with SpliceParents[Double] with DefaultIterationOutput[Double] with GriewankDouble

         val best = ga.execute()

         println(best)

         best
      }
   }
}
