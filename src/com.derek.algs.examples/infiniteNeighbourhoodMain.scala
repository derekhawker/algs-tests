package com.derek.algs.examples

import scala.util.Random
import com.derek.algs.util.TimedExecution
import com.derek.algs.{Scoring, Tabusearch, GeneticAlgorithm}
import com.derek.algs.structures.concrete.{INTraitSeqVal, FNTraitSeqVal, FNTraitSeqRef}
import com.derek.algs.structures.specification.TraitSeq

/**
 * @author Derek Hawker
 */
object infiniteNeighbourhoodMain {
  def main(args: Array[String]) {
    val numPopulation = 2800
    val numGenerations = 400
    val mutationRate = 0.4
    val tabuTimeToLive = 5
    val numFeatures = 10

    valTraitSequenceExamples(numPopulation, numGenerations, mutationRate, tabuTimeToLive, numFeatures)
  }


  def valTraitSequenceExamples(numPopulation: Int,
                               numGenerations: Int,
                               mutationRate: Double,
                               tabuTimeToLive: Int,
                               numFeatures: Int) {
    val population = Array.range(0, numPopulation)
      .map(person =>
      new INTraitSeqVal(Array.range(0, numFeatures)
        .map(tr => Random.nextInt(20) - 10),
      Array.range(0, numFeatures)
      .map(d=> (-10, 10))).asInstanceOf[TraitSeq[Int]])

    new TimedExecution().execute {
      val best = new GeneticAlgorithm[Int](population, numGenerations, mutationRate,
        endOfGenerationCondition, GeneticAlgorithm.Mating.eliteSelection,
        GeneticAlgorithm.BabyMaker.spliceParents, generationPrinter, Scoring.griewankInt)
        .execute()

      println(best)
      best
    }



    new TimedExecution().execute {
      val best = new Tabusearch[Int](population.head, tabuTimeToLive, numGenerations,
        endOfIterationCondition, iterationPrinter, Scoring.griewankInt)
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

  def generationPrinter[T](generation: Int,
                           population: Array[TraitSeq[T]],
                           scores: Array[Double],
                           globalBest: (TraitSeq[T], Double),
                           genBest: (TraitSeq[T], Double)) {
    println("^generation: " + generation)

    val mean: Double = scores.sum / scores.length
    println("\tmean: " + mean
      + ", std.dev: " + math.sqrt(scores.foldLeft(0.0)(
      (count, s) =>
        count + math.pow(s - mean, 2)) / scores.length))

    println("\t[Global]Best: (%f) %s".format(globalBest._2, globalBest._1))
    println("\t[Local]Best: (%f) %s".format(genBest._2, genBest._1))
  }


  def iterationPrinter[T](i: Int,
                          globalBest: TraitSeq[T],
                          globalBestScore: Double,
                          localBest: TraitSeq[T],
                          localBestScore: Double) {
    println("^iteration: " + i)
    println("\t[Global]Best: (%f) %s".format(globalBestScore, globalBest))
    println("\t[Local]Best: (%f) %s".format(localBestScore, localBest))
  }
}
