package examples


import java.io.{FileReader, BufferedReader}
import weka.core.Instances
import logging.Logger
import meta_heuristics.util.{Scoring, Output, TimedExecution}
import meta_heuristics.particle_swarm_optimization.particle.Particle
import meta_heuristics.structures.concrete.infinite.neighbourhood.{IntTraitSeqVal, DoubleTraitSeqVal}
import meta_heuristics.{Tabusearch, GeneticAlgorithm, ParticleSwarm}
import scala.util.Random
import meta_heuristics.structures.specification.TraitSeq


/**
 * @author Derek Hawker
 */
object InfiniteNeighbourhoodMain {
  def main(args: Array[String]) {
    val numPopulation = 2800
    val numGenerations = 400
    val mutationRate = 0.4
    val tabuTimeToLive = 5
    val numFeatures = 10

    doubleValTraitSequenceExamples(numPopulation, numGenerations, mutationRate, tabuTimeToLive,
      numFeatures)

    intValTraitSequenceExamples(numPopulation, numGenerations, mutationRate, tabuTimeToLive,
      numFeatures)
  }


  def intValTraitSequenceExamples(numPopulation: Int,
                                  numGenerations: Int,
                                  mutationRate: Double,
                                  tabuTimeToLive: Int,
                                  numFeatures: Int) {
    val population = Array.range(0, numPopulation)
      .map(person =>
      new IntTraitSeqVal(Array.range(0, numFeatures)
        .map(tr => Random.nextInt(20 - 10)),
        Array.range(0, numFeatures)
          .map(d => (-10, 10))).asInstanceOf[TraitSeq[Int]])

    new TimedExecution().execute {
      val best = new GeneticAlgorithm[Int](population, numGenerations, mutationRate,
        endOfGenerationCondition, GeneticAlgorithm.Mating.eliteSelection,
        GeneticAlgorithm.BabyMaker.spliceTwoParents, Output.defaultIterationPrinter, Scoring.griewankInt)
        .execute()

      println(best)
      best
    }

    new TimedExecution().execute {
      val best = new Tabusearch[Int](population.head, tabuTimeToLive, numGenerations,
        endOfIterationCondition, Output.defaultIterationPrinter, Scoring.griewankInt)
        .execute()

      println(best)
      best
    }
  }

  def doubleValTraitSequenceExamples(numPopulation: Int,
                                     numGenerations: Int,
                                     mutationRate: Double,
                                     tabuTimeToLive: Int,
                                     numFeatures: Int) {
    val population = Array.range(0, numPopulation)
      .map(person =>
      new DoubleTraitSeqVal(Array.range(0, numFeatures)
        .map(tr => Random.nextDouble() * 20 - 10),
        Array.range(0, numFeatures)
          .map(d => (-10.0, 10.0))).asInstanceOf[TraitSeq[Double]])

    new TimedExecution().execute {
      val best = new GeneticAlgorithm[Double](population, numGenerations, mutationRate,
        endOfGenerationCondition, GeneticAlgorithm.Mating.eliteSelection,
        GeneticAlgorithm.BabyMaker.spliceTwoParents, Output.defaultIterationPrinter, Scoring.griewank)
        .execute()

      println(best)
      best
    }

    new TimedExecution().execute {
      val best = new Tabusearch[Double](population.head, tabuTimeToLive, numGenerations,
        endOfIterationCondition, Output.defaultIterationPrinter, Scoring.griewank)
        .execute()

      println(best)
      best
    }
  }

  def endOfGenerationCondition[T](generation: Int,
                                  population: Array[TraitSeq[T]],
                                  scores: Array[Double]): Boolean = {
    true
  }

  def endOfIterationCondition[T](iteration: Int,
                                 globalBest: TraitSeq[T],
                                 globalBestScore: Double,
                                 localBest: TraitSeq[T],
                                 localBestScore: Double): Boolean = {
    true
  }

}
