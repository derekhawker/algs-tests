package examples.four_colour_17x17

import com.typesafe.scalalogging.slf4j.StrictLogging
import examples.four_colour_17x17.Main._
import meta_heuristics.genetic_algorithms.RandomMutation
import meta_heuristics.genetic_algorithms.babies.SpliceParents
import meta_heuristics.genetic_algorithms.population_selector.EliteSelection
import meta_heuristics.structures.concrete.finite.neighbourhood.TraitSeqVal
import meta_heuristics.structures.specification.TraitSeq
import meta_heuristics.structures.specification.TraitSeq
import meta_heuristics.structures.specification.TraitSeq
import meta_heuristics.structures.specification.TraitSeq
import meta_heuristics.structures.specification.TraitSeq
import meta_heuristics.structures.specification.TraitSeq
import meta_heuristics.structures.specification.TraitSeq
import meta_heuristics.util.TimedExecution
import meta_heuristics.util.gif.AnimatedProgressGif
import meta_heuristics.{GeneticAlgorithm, IgnoredIterationConditionCheck}

import scala.util.Random

/**
 * @author Derek Hawker
 */
object BenchmarkingTest
{
   def main(args: Array[String]): Unit =
   {

      val population = Array.range(0, Main.numGApopulation)
         .map(person =>
         new TraitSeqVal(Array.range(0, numFeatures)
            .map(tr =>
            Random.nextInt(4)),
            neighbourhood).asInstanceOf[TraitSeq[Int]])

      val ga = new GeneticAlgorithm[Int](population, Main.numGAgenerations,
         Main.mutationRate)
         with IgnoredIterationConditionCheck[Int] with EliteSelection[Int]
         with SpliceParents[Int] with NoOuputPrinter[Int] with IntScorerOptimized
         with RandomMutation[Int]

      var i = 0
      while (i < 10) {
         val best = new TimedExecution().execute {
            ga.execute()
         }
         i += 1
      }



   }

}

/**
 * @author Derek Hawker
 */
trait NoOuputPrinter[T] extends StrictLogging
{
   def printIteration(iteration: Int,
                      population: Array[TraitSeq[T]],
                      scores: Array[Double],
                      globalBest: TraitSeq[T],
                      globalBestScore: Double,
                      localBest: TraitSeq[T],
                      localBestScore: Double): Unit =
   {
   }
}






