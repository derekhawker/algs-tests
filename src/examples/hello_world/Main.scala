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
object Main
{
   val numPopulation  = 2800
   val numGenerations = 400
   val mutationRate   = 0.4
   val tabuTimeToLive = 5

   def main(args: Array[String]): Unit =
   {
      valTraitSequenceExamples()

      refTraitSequenceExamples()
   }


   private def valTraitSequenceExamples(): Unit =
   {
      val neighbourhood = Array.range(0, 10)
         .map(i =>
         Array.range('A', 'z').map(_.toChar))

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

      val gaBest = new TimedExecution().execute {
         ga.execute()
      }

      println(gaBest)


      val tbs = new Tabusearch[Char](population.head, tabuTimeToLive, numGenerations)
         with IgnoredIterationConditionCheck[Char] with DefaultIterationOutput[Char]
         with HelloWorldCharScorer

      val tbsBest = new TimedExecution().execute {
         tbs.execute()
      }

      println(tbsBest)
   }


   private def refTraitSequenceExamples(): Unit =
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



      val tbs = new Tabusearch[String](population.head, tabuTimeToLive, numGenerations)
         with IgnoredIterationConditionCheck[String] with DefaultIterationOutput[String]
         with HelloWorldStringScorer

      val tbsBest = new TimedExecution().execute {
         tbs.execute()
      }

      println(tbsBest)


      val ga = new GeneticAlgorithm[String](population, numGenerations, mutationRate)
         with IgnoredIterationConditionCheck[String] with EliteSelection[String]
         with SpliceParents[String] with DefaultIterationOutput[String] with HelloWorldStringScorer
         with RandomMutation[String]

      val gaBest = new TimedExecution().execute {
         ga.execute()
      }
      println(gaBest)
   }
}




