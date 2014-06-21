package examples

import java.io.{FileReader, BufferedReader}
import weka.core.Instances
import meta_heuristics.util.TimedExecution
import meta_heuristics.particle_swarm_optimization.particle.{DoublePositionUpdate, DoubleVelocityUpdate, Particle}
import meta_heuristics.structures.concrete.infinite.neighbourhood.DoubleTraitSeqVal
import meta_heuristics._
import scala.util.Random
import meta_heuristics.structures.specification.TraitSeq
import meta_heuristics.output.{DefaultIterationOutput, DefaultPSOIterationOutput}
import com.typesafe.scalalogging.slf4j.StrictLogging

/**
 * @author Derek Hawker
 */
object ClassifierMain extends StrictLogging
{
   val reader  = new BufferedReader(
      new FileReader("/home/derekhawker/programs/datasets/misc/sysc5405-train.arff"))
   val dataset = new Instances(reader)
   reader.close()
   // setting class attribute
   dataset.setClassIndex(dataset.numAttributes() - 1)

   val numInstances = dataset.numInstances()
   val numFeatures  = dataset.numAttributes()

   val featureUpperBound      = 50.0
   val featureNumericalBounds = Array.range(0, numFeatures)
      .map(tr => {
      val featureUpper: Double = featureUpperBound / 2
      val featureLower: Double = -featureUpperBound / 2
      (featureUpper, featureLower)
   })

   val rawInstancesData = instances2array(dataset)

   def main(args: Array[String])
   {
      //    gaTest
      //
      //    tabuTest

      psoTest
   }

   def psoTest
   {
      new TimedExecution().execute {

         val velocityFollow = 0.50
         val localOptimumFollow = 0.40
         val globalOptimumFollow = 0.73
         val numIterations = 400
         val numPopulation = 5

         val population = Array.range(0, numPopulation)
            .map(p => {
            val initWeights = Array.range(0, numFeatures)
               .map(m => Random.nextDouble() * featureUpperBound - featureUpperBound / 2.0)

            val initVelocity = Array.range(0, numFeatures)
               .map(
                  m => Random.nextDouble() * featureUpperBound / 100.0 - featureUpperBound / 200.0)

            new Particle[Double](new DoubleTraitSeqVal(initWeights, featureNumericalBounds),
               new DoubleTraitSeqVal(initVelocity, featureNumericalBounds),
               new DoubleTraitSeqVal(initWeights, featureNumericalBounds),
               null)
         })

         val positionBounds = Array.range(0, numFeatures)
            .map(m => (-featureUpperBound / 200.0, featureUpperBound / 200.0))

         val best = new ParticleSwarm[Double](population, positionBounds, velocityFollow,
            globalOptimumFollow, localOptimumFollow, numIterations) with DoubleVelocityUpdate
            with DoublePositionUpdate with IgnoredPSOCondition[Double]
            with DefaultPSOIterationOutput[Double]
         {
            override def traitScore(ts: TraitSeq[Double]): Double =
               classifyingScorer[Double](ts)
         }
            .execute()

         logger.info(best.toString)
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


         val best = new Tabusearch[Double](startingSolution, tabuTimeToLive, numIterations)
            with IgnoredIterationConditionCheck[Double] with DefaultIterationOutput[Double]
         {

            override def traitScore(ts: TraitSeq[Double]): Double =
               classifyingScorer[Double](ts)
         }
            .execute()

         logger.info(best.toString)
         best
      }
   }

   private def gaTest
   {
      new TimedExecution().execute {
         val numGenerations = 300
         val numPopulation = 500
         val mutationRate = 0.4


         val population = Array.range(0, numPopulation)
            .map(person =>
            new DoubleTraitSeqVal(Array.range(0, numFeatures)
               .map(tr =>
               Random.nextDouble() * featureUpperBound - featureUpperBound / 2.0),
               featureNumericalBounds).asInstanceOf[TraitSeq[Double]])


         val best = GeneticAlgorithm.defaultArguments[Double](population, classifyingScorer)
            .execute()

         logger.info(best.toString)

         best
      }
   }

   def classifyingScorer[T](traitsequeunce: TraitSeq[T]): Double =
   {

      val ts = traitsequeunce.asInstanceOf[DoubleTraitSeqVal]
      val totalCorrect = (0 until numInstances)
         .foldLeft(0)(
            (totalCorrect, i) => {
               val hyperplaneTest = ts.zip(rawInstancesData(i)).foldLeft(0.0)(
                  (summ, a) => {
                     summ + a._1 * a._2
                  })

               totalCorrect +
                  (if (rawInstancesData(i)(numFeatures - 1) == 0.0 && hyperplaneTest <= 0.0 ||
                     rawInstancesData(i)(numFeatures - 1) == 1.0 && hyperplaneTest >= 0.0)
                     1
                  else
                     0)
            })

      -totalCorrect
   }

   def instances2array(instances: Instances): Array[Array[Double]] =
   {
      val data = Array.ofDim[Double](instances.numInstances(), instances.numAttributes())
      (0 until instances.numInstances())
         .foreach(i => {
         (0 until instances.numAttributes())
            .foreach(j => {
            data(i)(j) = instances.instance(i).value(j)
         })
      })

      data
   }

}
