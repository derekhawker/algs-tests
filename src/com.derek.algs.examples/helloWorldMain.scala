package com.derek.algs.examples

import com.derek.algs.{Tabusearch, GeneticAlgorithm}
import com.derek.algs.util.{Output, TimedExecution}
import com.derek.algs.structures.specification.TraitSeq
import scala.util.Random
import com.derek.algs.structures.concrete.finite.neighbourhood.{TraitSeqVal, TraitSeqRef}


/**
 * @author Derek Hawker
 */
object HelloWorldMain {
  def main(args: Array[String]): Unit = {
    val numPopulation = 2800
    val numGenerations = 400
    val mutationRate = 0.4
    val tabuTimeToLive = 5

    valTraitSequenceExamples(numPopulation, numGenerations, mutationRate, tabuTimeToLive)
    refTraitSequenceExamples(numPopulation, numGenerations, mutationRate, tabuTimeToLive)
  }


  private def valTraitSequenceExamples(numPopulation: Int,
                                       numGenerations: Int,
                                       mutationRate: Double,
                                       tabuTimeToLive: Int) {
    val neighbourhood = Array.range(0, 10)
      .map(i =>
      Array.range('A', 'z').map(_.toChar)
      )

    val population = Array.range(0, numPopulation)
      .map(person =>
      new TraitSeqVal(Array.range(0, 10)
        .map(tr =>
        ('A' + Random.nextInt('z' - 'A')).toChar),
        neighbourhood).asInstanceOf[TraitSeq[Char]])

    new TimedExecution().execute {
      val best = new GeneticAlgorithm[Char](population, numGenerations, mutationRate,
        endOfGenerationCondition, GeneticAlgorithm.Mating.eliteSelection,
        GeneticAlgorithm.BabyMaker.spliceTwoParents, Output.gaGenerationPrinter, helloWorldCharScorer)
        .execute()

      println(best)
      best
    }



    new TimedExecution().execute {
      val best = new Tabusearch[Char](population.head, tabuTimeToLive, numGenerations,
        endOfIterationCondition, Output.tabusearchIterationPrinter, helloWorldCharScorer)
        .execute()

      println(best)
      best
    }
  }


  private def refTraitSequenceExamples(numPopulation: Int,
                                       numGenerations: Int,
                                       mutationRate: Double,
                                       tabuTimeToLive: Int) {
    val neighbourhood = Array.range(0, 10)
      .map(i =>
      Array.range('A', 'z').map(_.toChar.toString)
      )

    val population = Array.range(0, numPopulation)
      .map(person =>
      new TraitSeqRef[String](Array.range(0, 10)
        .map(tr =>
        ('A' + Random.nextInt('z' - 'A')).toChar.toString), neighbourhood,
        (c: String) => new String(c)).asInstanceOf[TraitSeq[String]]
      )



    new TimedExecution().execute {
      val best = new GeneticAlgorithm[String](population, numGenerations, mutationRate,
        endOfGenerationCondition, GeneticAlgorithm.Mating.eliteSelection,
        GeneticAlgorithm.BabyMaker.spliceTwoParents, Output.gaGenerationPrinter,
        helloWorldStringScorer)
        .execute()

      println(best)
      best
    }




    new TimedExecution().execute {
      val best = new Tabusearch[String](population.head, tabuTimeToLive, numGenerations,
        endOfIterationCondition, Output.tabusearchIterationPrinter, helloWorldStringScorer)
        .execute()

      println(best)
      best
    }
  }

  def endOfGenerationCondition[T](generation: Int,
                                  population: Array[TraitSeq[T]],
                                  scores: Array[Double]): Boolean = {
    if (scores.forall(d => math.abs(d - 0.0) > 0.0000001))
      true
    else
      false
  }

  private def endOfIterationCondition[T](iteration: Int,
                                         globalBest: TraitSeq[T],
                                         globalBestScore: Double,
                                         localBest: TraitSeq[T],
                                         localBestScore: Double): Boolean = {
    if ((math.abs(localBestScore - 0.0) < 0.0000001)
      || (math.abs(globalBestScore - 0.0) < 0.0000001))
      false
    else
      true
  }


  private def helloWorldStringScorer(traitsequence: TraitSeq[String]): Double = {
    val ts = traitsequence.asInstanceOf[TraitSeqRef[String]]

    "HelloWorld".zip(ts).foldLeft(0.0)(
      (score, zipped) => {
        val perfectChar = zipped._1
        val tsChar = zipped._2

        score - math.abs(perfectChar - tsChar.toCharArray()(0))
      }
    )
  }

  private def helloWorldCharScorer(traitsequence: TraitSeq[Char]): Double = {
    val ts = traitsequence.asInstanceOf[TraitSeqVal[Char]]

    "HelloWorld".zip(ts).foldLeft(0.0)(
      (score, zipped) => {
        val perfectChar = zipped._1
        val tsChar = zipped._2

        score - math.abs(perfectChar - tsChar)
      }
    )
  }
}
