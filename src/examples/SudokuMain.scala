package examples

import meta_heuristics.{GeneticAlgorithm, IgnoredIterationConditionCheck, Tabusearch}
import meta_heuristics.util.TimedExecution
import meta_heuristics.structures.concrete.finite.neighbourhood.TraitSeqVal
import scala.util.Random
import meta_heuristics.structures.specification.TraitSeq
import meta_heuristics.output.{DefaultBranchAndBoundIterationOutput, DefaultIterationOutput}
import meta_heuristics.genetic_algorithms.population_selector.EliteSelection
import meta_heuristics.genetic_algorithms.babies.SpliceParents
import examples.four_colour_17x17.FeasibleSolutionCheck
import meta_heuristics.genetic_algorithms.RandomMutation
import optimization.BranchAndBound
import com.typesafe.scalalogging.slf4j.StrictLogging

/**
 * @author Derek Hawker
 */
object SudokuMain extends StrictLogging
{
   val numGApopulation  = 52
   val numGAgenerations = 200
   val mutationRate     = 0.4

   val numFeatures    = 10 * 10
   val numIterations  = 400
   val tabuTimeToLive = 5

   val neighbourhood = Array.range(0, numFeatures)
      .map(i =>
      Array.range(0, 10))


   def main(args: Array[String]): Unit =
   {
      bnbTest()
   }

   def bnbTest(): Unit =
   {
      val pattern = Array.range(10, numFeatures + 10)
         .map(i => i)

      val patternSolution = new TraitSeqVal[Int](pattern, neighbourhood)
      logger.info(patternSolution.toString)

      val bnb = new BranchAndBound[Int](patternSolution, BranchAndBound.depthFirstOrdering,
         None, neighbourhood)
         with SudokuScorer with DefaultBranchAndBoundIterationOutput[Int]
         with FeasibleSolutionCheck with BoundingFunction

      val best = new TimedExecution().execute {
         bnb.execute()
      }.asInstanceOf[Option[TraitSeq[Int]]]

      logger.info(best.toString)
   }

   def gaTest(): Unit =
   {
      val population = Array.range(0, numGApopulation)
         .map(person =>
         new TraitSeqVal(Array.range(0, numFeatures)
            .map(tr =>
            Random.nextInt(10)),
            neighbourhood).asInstanceOf[TraitSeq[Int]])

      val ga = new GeneticAlgorithm[Int](population, numGAgenerations,
         mutationRate)
         with IgnoredIterationConditionCheck[Int] with EliteSelection[Int]
         with SpliceParents[Int] with DefaultIterationOutput[Int] with SudokuScorer
         with RandomMutation[Int]

      val best = new TimedExecution().execute {
         ga.execute()
      }

      logger.info(best.toString)

   }

   def tabuTest(): Unit =
   {

      val startingSolution =
         new TraitSeqVal(Array.range(0, numFeatures)
            .map(tr =>
            Random.nextInt(10)),
            neighbourhood).asInstanceOf[TraitSeq[Int]]

      val tbs = new Tabusearch[Int](startingSolution, tabuTimeToLive, numIterations)
         with IgnoredIterationConditionCheck[Int] with DefaultIterationOutput[Int]
         with SudokuScorer


      val best = new TimedExecution().execute {
         tbs.execute()
      }

      logger.info(best.toString)
   }
}


trait SudokuScorer
{

   final def boundedTraitScore(ts: TraitSeq[Int],
                               level: Int): Double =
   {
      // TODO: Tired. Look at tomorrow.
      if(level < 99)
         0 - (20+traitScore(ts))
      else
         0
   }

   final def traitScore(ts: TraitSeq[Int]): Double =
   {
      val totalCorrecRows =
         (0 until 10).foldLeft(0)((correctRows, row) => {

            val included = Array.range(0, 10).map(i => false)

            (0 until 10).foreach(col => {
               val index = row * 10 + col
               val tsIndex = ts(index)

               if (tsIndex < 10)
                  included(tsIndex) = true
            })

            if (included.forall(i => i))
               correctRows + 1
            else
               correctRows
         })

      val totalCorrectCols =
         (0 until 10).foldLeft(0)((correctCols, col) => {

            val included = Array.range(0, 10).map(i => false)

            (0 until 10).foreach(row => {
               val index = row * 10 + col
               val tsIndex = ts(index)

               if (tsIndex < 10)
                  included(tsIndex) = true
            })

            if (included.forall(i => i))
               correctCols + 1
            else
               correctCols
         })

      -20 + (totalCorrecRows + totalCorrectCols)
   }
}

trait BoundingFunction extends BranchAndBound[Int]
{
   final def boundingTrait(ts: TraitSeq[Int],
                           branchLevel: Int,
                           decisionVariableValue: Int): TraitSeq[Int] =
   {
      val updatedSol = ts.deepcopy()
      updatedSol(branchLevel) = variableBounds(branchLevel)(decisionVariableValue)

      updatedSol
   }
}
