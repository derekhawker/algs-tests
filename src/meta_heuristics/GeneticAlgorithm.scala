package meta_heuristics

import scala.util.Random
import meta_heuristics.structures.specification.TraitSeq
import meta_heuristics.util.ExecutableAlgorithm
import meta_heuristics.genetic_algorithms.population_selector.EliteSelection
import meta_heuristics.genetic_algorithms.babies.SpliceParents
import meta_heuristics.output.DefaultIterationOutput
import meta_heuristics.genetic_algorithms.RandomMutation


/**
 * Generic Genetic Algorithm that operates on a population of TraitSeqs. At each iteration, a
 * population selector determines the subset of the population to survive. A mating function can
 * determine if a population of children should be created and added to the fit population. These
 * populations are subjected to a mutation function that is triggered by a mutation rate.
 *
 * @param population The initial starting population. Can be any kind of class that derives from the
 *                   base TraitSeq[T] class.
 * @param numIterations The maximum number of iterations to simulate for.
 * @param mutationRate (0.0 - 1.0) The chance of random mutation at each iteration. Affects the
 *                     parents and children.
 * @param endOfIterationCondition A condition that will end the simulation early, such as
 *                                solution convergence or reduction in population variance. Takes
 *                                as parameters:
 *                                - the iteration number
 *                                - The current population
 *                                - the scores of the current population
 *                                Returns
 *                                - true, if simulation should continue.
 *                                - false, if the simulation should end.
 *
 * @param fitPopulation Function that finds the fittest subset of population to use in next
 *                      iteration. Takes as parameters:
 *                      - fittest population
 *                      - the scores for each member of the population
 *                      Returns:
 *                      - 2d array, where each row is mating pair.
 * @param makeBabies Function that creates new members of population using the fit population.
 *                   Takes as parameters:
 *                   - mating pair (any number of population members)
 *                   Returns:
 *                   - a number of children
 * @param iterationOutputPrinter function that outputs information at each iteration. Takes as
 *                               parameters:
 *                               - iteration number
 *                               - population at start of iteration
 *                               - the scores for each member of the population
 *                               - the global best for all iterations
 *                               - the score of the global best
 *                               - the local best (from the the current population)
 *                               - the score of the local best
 *                               Returns Unit.
 * @param scorer function that evaluates a member of population. Takes as parameters:
 *               - the solution to evaluate
 *               Returns a Double representing the score. A higher score is the most positive value.
 * @tparam T conforms to the type of TraitSeq
 *
 * @author Derek Hawker
 */
abstract class GeneticAlgorithm[T](var population: Array[TraitSeq[T]],
                                   val numIterations: Int,
                                   val mutationRate: Double)
   extends ExecutableAlgorithm[T] with Serializable
{

   var currentIteration = 0
   val filename         = "ga.ser"

   /* must have an even number of parents.
    98 = bad because mating population is 49. 1 leftover parent.
    100 = good because  mating population is 50
    */
   assert(population.length % 4 == 0)
   assert(mutationRate >= 0.0 && mutationRate <= 1.0)

   protected def checkForConvergence(iteration: Int,
                           population: Array[TraitSeq[T]],
                           score: Array[Double]): Boolean

   protected def fitPopulation(traits: Array[TraitSeq[T]],
                     scores: Array[Double]): Array[Array[TraitSeq[T]]]

   protected def makeBabies(parents: Array[TraitSeq[T]]): Array[TraitSeq[T]]

   /**
    * Mutates the given trait sequence
    *
    * @param ts
    * @return mutated copy of original
    */
   protected def mutate(ts: TraitSeq[T]): TraitSeq[T]

   protected def printIteration(iteration: Int,
                      population: Array[TraitSeq[T]],
                      scores: Array[Double],
                      globalBest: TraitSeq[T],
                      globalBestScore: Double,
                      localBest: TraitSeq[T],
                      localBestScore: Double)

   protected def traitScore(ts: TraitSeq[T]): Double

   /**
    * Run GA simulation based on population and other metrics given when creating new GA object.
    *
    * @return Best TraitSeq found.
    */
   def execute(): Option[TraitSeq[T]] =
   {
      val res = innerExecute()

      population = res._1 // Save the last evolved population

      // Return the very highest scoring trait sequence
      val globalBest = res._2
      Some(globalBest._1)
   }


   /**
    *
    * @return
    */
   private def innerExecute(): (Array[TraitSeq[T]], (TraitSeq[T], Double), (TraitSeq[T], Double)) =
   {
      (currentIteration until (currentIteration + numIterations))
         .foldLeft(population,
            (population(0), Double.NegativeInfinity),
            (population(0), Double.NegativeInfinity))(
            (state, g) => {

               val pop = state._1
               val global: (TraitSeq[T], Double) = state._2
               val globalBest = global._1
               val globalBestScore = global._2

               val lastGen: (TraitSeq[T], Double) = state._3
               val lastGenBest = lastGen._1
               val lastGenBestScore = lastGen._2

               currentIteration += 1

               // Score all population
               val scores = pop.par.map(traitScore).toArray
               val genBest = pop.zip(scores)
                  .sortWith(_._2 > _._2)(0)
               val genBestScore = genBest._2
               val genBestTrait = genBest._1

               printIteration(g, pop, scores, globalBest, globalBestScore, genBestTrait,
                  genBestScore)


               /** **************************************************************************************
                 * Early exit if meeting certain conditions.
                 * This appears halfway in execution because a new iteration can't be made without
                 * first scoring the babies made from last iteration. And it makes no sense to make a
                 * new iteration, not score them, and THEN exit.
                 */
               val canContinue = checkForConvergence(g, pop, scores)
               if (!canContinue)
                  if (genBestScore > globalBestScore)
                     return (pop, genBest, genBest)
                  else
                     return (pop, global, genBest)

               /** *************************************************************************************/


               // Apply selection method to find breeding population
               val breedingPop = fitPopulation(pop, scores)
               // Make babies
               val children = breedingPop.par.map(makeBabies).flatten.toArray
               // Mutate them
               val newpop = breedingPop.flatten ++ children
               val finalpop = newpop.map(mutate)

               if (genBestScore > globalBestScore)
                  (finalpop, genBest, genBest)
               else
                  (finalpop, global, genBest)

            })
   }
}


/**
 * @author Derek Hawker
 */
object GeneticAlgorithm
{


   def defaultArguments[T](population: Array[TraitSeq[T]],
                           score: TraitSeq[T] => Double): GeneticAlgorithm[T] =
   {
      val mutationRate = 0.99
      val numIterations = 2000

      new GeneticAlgorithm[T](population, numIterations, mutationRate)
         with IgnoredIterationConditionCheck[T] with EliteSelection[T] with SpliceParents[T]
         with DefaultIterationOutput[T] with RandomMutation[T]
      {
         override def traitScore(ts: TraitSeq[T]): Double =
         {
            score.apply(ts)
         }
      }
   }


}
