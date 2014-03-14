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
      val best = new GeneticAlgorithm[String](population, numGenerations, mutationRate, helloWorldScorer,
        rng).run()
      println(best)
      best
    }


    val tabuTimeToLive = 5

    TimedExecution.run {
      val best = new Tabusearch(population.head, tabuTimeToLive, numGenerations, helloWorldScorer,
        rng).run()
      println(best)
      best
    }

  }

  def helloWorldScorer(traitsequence: TraitSequence[String]): Double = {
    val ts = traitsequence.asInstanceOf[TraitSequenceRef[String]]

    "HelloWorld".zip(ts).foldLeft(0.0)(
      (score, zipped) => {
        val perfectChar = zipped._1
        val tsChar = zipped._2

        score - math.abs(perfectChar - tsChar.toCharArray()(0))
      }
    )
  }

}
