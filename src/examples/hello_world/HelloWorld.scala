package examples.hello_world

import meta_heuristics.util.TimedExecution
import meta_heuristics.{IgnoredIterationConditionCheck, Tabusearch, GeneticAlgorithm}
import scala.util.Random
import meta_heuristics.structures.specification.TraitSeq
import meta_heuristics.structures.concrete.finite.neighbourhood.{TraitSeqRef, TraitSeqVal}
import meta_heuristics.genetic_algorithms.population_selector.EliteSelection
import meta_heuristics.genetic_algorithms.babies.SpliceParents
import meta_heuristics.output.DefaultIterationOutput
import meta_heuristics.genetic_algorithms.RandomMutation

/**
 * @author Derek Hawker
 */
object HelloWorld
{
   def main(args: Array[String]): Unit =
   {
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
                                        tabuTimeToLive: Int)
   {
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

      val ga =
         new GeneticAlgorithm[Char](population, numGenerations, mutationRate)
            with IgnoredIterationConditionCheck[Char] with EliteSelection[Char] with SpliceParents[Char]
            with DefaultIterationOutput[Char] with HelloWorldCharScorer with RandomMutation[Char]


      new TimedExecution().execute {
         val best = ga.execute()
         println(best)
         best
      }
      new TimedExecution().execute {
         val best = ga.execute()
         println(best)
         best
      }


      new TimedExecution().execute {
         val tbs = new Tabusearch[Char](population.head, tabuTimeToLive, numGenerations)
            with IgnoredIterationConditionCheck[Char] with DefaultIterationOutput[Char]
            with HelloWorldCharScorer

         val best = tbs.execute()

         println(best)
         best
      }
   }


   private def refTraitSequenceExamples(numPopulation: Int,
                                        numGenerations: Int,
                                        mutationRate: Double,
                                        tabuTimeToLive: Int)
   {
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
         val tbs = new GeneticAlgorithm[String](population, numGenerations, mutationRate)
            with IgnoredIterationConditionCheck[String] with EliteSelection[String]
            with SpliceParents[String] with DefaultIterationOutput[String] with HelloWorldStringScorer
            with RandomMutation[String]

         val best = tbs.execute()

         println(best)
         best
      }




      new TimedExecution().execute {
         val tbs = new Tabusearch[String](population.head, tabuTimeToLive, numGenerations)
            with IgnoredIterationConditionCheck[String] with DefaultIterationOutput[String]
            with HelloWorldStringScorer

         val best = tbs.execute()

         println(best)
         best
      }
   }

   def endOfGenerationCondition[T](generation: Int,
                                   population: Array[TraitSeq[T]],
                                   scores: Array[Double]): Boolean =
   {
      if (scores.forall(d => math.abs(d - 0.0) > 0.0000001))
         true
      else
         false
   }

   private def endOfIterationCondition[T](iteration: Int,
                                          globalBest: TraitSeq[T],
                                          globalBestScore: Double,
                                          localBest: TraitSeq[T],
                                          localBestScore: Double): Boolean =
   {
      if ((math.abs(localBestScore - 0.0) < 0.0000001)
         || (math.abs(globalBestScore - 0.0) < 0.0000001))
         false
      else
         true
   }
}




