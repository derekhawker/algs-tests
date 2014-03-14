package com.derek.algs.examples

import com.derek.algs.structures.{TraitSequenceRef, TraitSequence, TraitSequenceVal}
import scala.util.Random
import com.derek.algs.util.TimedExecution
import com.derek.algs.{Tabusearch, GeneticAlgorithm}

/**
 * @author Derek Hawker
 */
object helloWorldMain {
  def main(args: Array[String]) {
    val rng = new Random()
    val numPopulation = 28
    val numGenerations = 400
    val mutationRate = 0.4
    val tabuTimeToLive = 5

    valTraitSequenceExamples(numPopulation, rng, numGenerations, mutationRate, tabuTimeToLive)
    refTraitSequenceExamples(numPopulation, rng, numGenerations, mutationRate, tabuTimeToLive)
  }


  def valTraitSequenceExamples(numPopulation: Int, rng: Random, numGenerations: Int,
                               mutationRate: Double, tabuTimeToLive: Int) {
    val neighbourhood = Array.range(0, 10)
      .map(i =>
      Array.range('A', 'z').map(_.toChar)
      )

    val population = Array.range(0, numPopulation)
      .map(person =>
      new TraitSequenceVal(Array.range(0, 10)
        .map(
          tr =>
            ('A' + rng.nextInt('z' - 'A')).toChar),
        neighbourhood).asInstanceOf[TraitSequence[Char]])

    TimedExecution.run {
      val best = new GeneticAlgorithm[Char](population, numGenerations, mutationRate, rng,
        endOfGenerationCondition, helloWorldCharScorer).run()
      println(best)
      best
    }



    TimedExecution.run {
      val best = new Tabusearch[Char](population.head, tabuTimeToLive, numGenerations,
        rng, endOfIterationCondition, helloWorldCharScorer).run()
      println(best)
      best
    }
  }


  def refTraitSequenceExamples(numPopulation: Int, rng: Random, numGenerations: Int,
                               mutationRate: Double, tabuTimeToLive: Int) {
    val neighbourhood = Array.range(0, 10)
      .map(i =>
      Array.range('A', 'z').map(_.toChar.toString)
      )

    val population = Array.range(0, numPopulation)
      .map(person =>
      new TraitSequenceRef[String](Array.range(0, 10)
        .map(
          tr =>
            ('A' + rng.nextInt('z' - 'A')).toChar.toString
        ), neighbourhood,
        (c: String) => new String(c)).asInstanceOf[TraitSequence[String]]
      )



    TimedExecution.run {
      val best = new GeneticAlgorithm[String](population, numGenerations, mutationRate, rng,
        endOfGenerationCondition, helloWorldStringScorer).run()
      println(best)
      best
    }



    TimedExecution.run {
      val best = new Tabusearch[String](population.head, tabuTimeToLive, numGenerations,
        rng, endOfIterationCondition, helloWorldStringScorer).run()
      println(best)
      best
    }
  }

  def endOfGenerationCondition[T](generation: Int,
                                  population: Array[TraitSequence[T]],
                                  scores: Array[Double]): Boolean = {
    if (scores.forall(d => math.abs(d - 0.0) > 0.0000001))
      true
    else
      false
  }

  def endOfIterationCondition[T](iteration: Int,
                                 globalBest: TraitSequence[T],
                                 globalBestScore: Double,
                                 localBest: TraitSequence[T],
                                 localBestScore: Double): Boolean = {
    if ((math.abs(localBestScore - 0.0) < 0.0000001)
      || (math.abs(globalBestScore - 0.0) < 0.0000001))
      false
    else
      true
  }

  def helloWorldStringScorer(traitsequence: TraitSequence[String]): Double = {
    val ts = traitsequence.asInstanceOf[TraitSequenceRef[String]]

    "HelloWorld".zip(ts).foldLeft(0.0)(
      (score, zipped) => {
        val perfectChar = zipped._1
        val tsChar = zipped._2

        score - math.abs(perfectChar - tsChar.toCharArray()(0))
      }
    )
  }

  def helloWorldCharScorer(traitsequence: TraitSequence[Char]): Double = {
    val ts = traitsequence.asInstanceOf[TraitSequenceVal[Char]]

    "HelloWorld".zip(ts).foldLeft(0.0)(
      (score, zipped) => {
        val perfectChar = zipped._1
        val tsChar = zipped._2

        score - math.abs(perfectChar - tsChar)
      }
    )
  }

}
