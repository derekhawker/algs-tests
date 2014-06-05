package examples

import meta_heuristics.util.TimedExecution
import meta_heuristics.structures.concrete.infinite.neighbourhood.{IntTraitSeqVal, DoubleTraitSeqVal}
import meta_heuristics.{IgnoredGeneticAlgorithmCondition, Tabusearch, GeneticAlgorithm}
import scala.util.Random
import meta_heuristics.structures.specification.TraitSeq
import meta_heuristics.output.DefaultIterationOutput
import meta_heuristics.scoring.{GriewankDouble, GriewankInt}
import meta_heuristics.genetic_algorithms.population_selector.EliteSelection
import meta_heuristics.genetic_algorithms.babies.SpliceParents


/**
 * @author Derek Hawker
 */
object InfiniteNeighbourhoodMain
{
  def main(args: Array[String])
  {
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
                                  numFeatures: Int)
  {
    val population = Array.range(0, numPopulation)
      .map(person =>
      new IntTraitSeqVal(Array.range(0, numFeatures)
        .map(tr => Random.nextInt(20 - 10)),
        Array.range(0, numFeatures)
          .map(d => (-10, 10))).asInstanceOf[TraitSeq[Int]])

    new TimedExecution().execute {
      val ga = new GeneticAlgorithm[Int](population, numGenerations, mutationRate)
        with IgnoredGeneticAlgorithmCondition[Int] with EliteSelection[Int] with SpliceParents[Int]
        with DefaultIterationOutput[Int] with GriewankInt
      val best = ga.execute()

      println(best)
      best
    }

    new TimedExecution().execute {
      val tbs = new Tabusearch[Int](population.head, tabuTimeToLive, numGenerations)
        with IgnoredGeneticAlgorithmCondition[Int] with DefaultIterationOutput[Int] with
        GriewankInt
      val best = tbs.execute()

      println(best)
      best
    }
  }

  def doubleValTraitSequenceExamples(numPopulation: Int,
                                     numGenerations: Int,
                                     mutationRate: Double,
                                     tabuTimeToLive: Int,
                                     numFeatures: Int)
  {
    val population = Array.range(0, numPopulation)
      .map(person =>
      new DoubleTraitSeqVal(Array.range(0, numFeatures)
        .map(tr => Random.nextDouble() * 20 - 10),
        Array.range(0, numFeatures)
          .map(d => (-10.0, 10.0))).asInstanceOf[TraitSeq[Double]])

    new TimedExecution().execute {
      val ga = new GeneticAlgorithm[Double](population, numGenerations, mutationRate)
        with IgnoredGeneticAlgorithmCondition[Double] with EliteSelection[Double]
        with SpliceParents[Double] with DefaultIterationOutput[Double] with GriewankDouble
      val best = ga.execute()

      println(best)
      best
    }

    new TimedExecution().execute {
      val tbs = new Tabusearch[Double](population.head, tabuTimeToLive, numGenerations)
        with IgnoredGeneticAlgorithmCondition[Double] with DefaultIterationOutput[Double]
        with GriewankDouble
      val best = tbs.execute()

      println(best)
      best
    }
  }
}
