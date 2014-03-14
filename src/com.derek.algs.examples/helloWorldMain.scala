package com.derek.algs.examples

import com.derek.algs.structures.{TraitSequence, TraitSequenceVal}
import scala.util.Random
import com.derek.algs.util.TimedExecution
import com.derek.algs.{Tabusearch, GeneticAlgorithm}

/**
 * @author Derek Hawker
 */
object helloWorldMain {
  def main(args: Array[String]) {
    val rng = new Random()
    val numPopulation = 12
    val numGenerations = 400
    val mutationRate = 0.4

    val neighbourhood = Array.range(0, 10)
      .map(i =>
      Array.range('A', 'z').map(_.toChar)
      )

    val population = Array.range(0, numPopulation)
      .map(person =>
      new TraitSequenceVal(Array.range(0, 10)
        .map(
          tr =>
            ('A' + rng.nextInt('z' - 'A')).toChar
        ), neighbourhood).asInstanceOf[TraitSequence[Char]]
      )

    TimedExecution.run {
      val best = new GeneticAlgorithm(population, numGenerations, mutationRate, helloWorldScorer,
        rng).run
      println(best)
      best
    }


    val tabuTimeToLive = 5

    TimedExecution.run {
      val best = new Tabusearch(population.head, tabuTimeToLive, numGenerations, helloWorldScorer,
        rng).run
      println(best)
      best
    }

  }

  def helloWorldScorer(traitsequence: TraitSequence[Char]): Double = {
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
