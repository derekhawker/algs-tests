package com.derek.algs.examples

import weka.core.Instances
import java.io.{FileReader, BufferedReader}
import com.derek.algs.util.{Output, TimedExecution}
import com.derek.algs.particle.swarm.optimization.Particle
import com.derek.algs.{GeneticAlgorithm, Tabusearch, ParticleSwarmOptimization}
import com.derek.algs.structures.concrete.infinite.neighbourhood.DoubleTraitSeqVal
import scala.util.Random
import com.derek.algs.structures.specification.TraitSeq

/**
 * @author Derek Hawker
 */
object ClassifierMain {
  val reader  = new BufferedReader(
    new FileReader("./datasets/sysc5405-train.arff"))
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


  def main(args: Array[String]) {


    gaTest


    tabuTest


    psoTest
  }

  def psoTest {
    new TimedExecution().execute {

      val velocityFollow = 0.50
      val localOptimumFollow = 0.40
      val globalOptimumFollow = 0.73
      val numIterations = 400
      val numPopulation = 5
      val numGaGenerations = 300
      val numGaPopulation = 5000
      val mutationRate = 0.4
      val tabuTimeToLive = 5

      val population = Array.range(0, numPopulation)
        .map(p => {
        val initWeights = Array.range(0, numFeatures)
          .map(m => Random.nextDouble() * featureUpperBound - featureUpperBound / 2.0)

        val initVelocity = Array.range(0, numFeatures)
          .map(m => Random.nextDouble() * featureUpperBound / 100.0 - featureUpperBound / 200.0)

        new Particle[Double](new DoubleTraitSeqVal(initWeights, featureNumericalBounds),
          new DoubleTraitSeqVal(initVelocity, featureNumericalBounds),
          new DoubleTraitSeqVal(initWeights, featureNumericalBounds))
      })

      val best = new ParticleSwarmOptimization[Double](population, velocityFollow,
        globalOptimumFollow, localOptimumFollow, numIterations, Particle.updateVelocityDouble,
        endOfPsoIterationCondition, Output.psoIterationPrinter, classifyingScorer)
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
        endOfIterationCondition, Output.defaultIterationPrinter, classifyingScorer)
        .execute()

      println(best)
      best
    }
  }

  private def gaTest {
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


      val best = new GeneticAlgorithm[Double](population, numGenerations, mutationRate,
        endOfGenerationCondition, GeneticAlgorithm.Mating.eliteSelection,
        GeneticAlgorithm.BabyMaker.spliceTwoParents, Output.defaultIterationPrinter,
        classifyingScorer)
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

  def classifyingScorer[T](traitsequeunce: TraitSeq[T]): Double = {

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

  def instances2array(instances: Instances): Array[Array[Double]] = {
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