package meta_heuristics

import scala.util.Random
import meta_heuristics.structures.specification.TraitSeq
import meta_heuristics.util.{Output, ExecutableAlgorithm}


/**
 * @author Derek Hawker
 */
object GeneticAlgorithm {

  def endOfGenerationCondition[T](generation: Int,
                                  population: Array[TraitSeq[T]],
                                  scores: Array[Double]): Boolean = {
    true
  }

  def defaultArguments[T](population: Array[TraitSeq[T]],
                          scorer: TraitSeq[T] => Double): GeneticAlgorithm[T] = {
    val mutationRate = 0.99
    val numGenerations = 2000

    new GeneticAlgorithm[T](population, numGenerations, mutationRate,
      endOfGenerationCondition, GeneticAlgorithm.Mating.eliteSelection,
      GeneticAlgorithm.BabyMaker.spliceTwoParents, Output.defaultIterationPrinter, scorer)
  }


  /**
   * @author Derek Hawker
   */
  object Mating {
    def eliteSelection[T](traits: Array[TraitSeq[T]],
                          scores: Array[Double]): Array[Array[TraitSeq[T]]] = {

      val sorted = traits.zip(scores).sortWith(_._2 > _._2)

      val matingPop = sorted.map(_._1)
        .slice(0, sorted.length / 2)

      Array.range(0, matingPop.length, 2)
        .map(i =>
        Array(matingPop(i), matingPop(i + 1))
        )
    }
  }

  /**
   * @author Derek Hawker
   */
  object BabyMaker {

    /**
     * Create children based on splicing
     *
     * @param parents
     * @return
     */
    def spliceTwoParents[T](parents: Array[TraitSeq[T]]): Array[TraitSeq[T]] = {
      assert(parents.length == 2)

      val (p1, p2) = (parents(0), parents(1))
      assert(p1.length == p2.length) // Condition that they both have same length

      val splitPoint = Random.nextInt(p1.length) + 1

      val child1 = p1.deepcopy()
      val child2 = p2.deepcopy()

      /* TODO: Probably broken for problems where we have reference types and the neighbourhood isn't
      finite. Like searching for weights. If you assign directly to a reference and then modify the
      reference, then you're going to modify a trait in two different traitsequences
       */
      (0 until splitPoint)
        .foreach(i =>
        child2(i) = child1(i))

      (splitPoint until p1.length)
        .foreach(i =>
        child1(i) = child2(i))

      Array(child1, child2)
    }
  }

}


/**
 * Generic Genetic Algorithm that operates on a population of TraitSeqs. At each iteration, a
 * population selector determines the subset of the population to survive. A mating function can
 * determine if a population of children should be created and added to the fit population. These
 * populations are subjected to a mutation function that is triggered by a mutation rate.
 *
 * @param population The initial starting population. Can be any kind of class that derives from the
 *                   base TraitSeq[T] class.
 * @param numGenerations The maximum number of generations to simulate for.
 * @param mutationRate (0.0 - 1.0) The chance of random mutation at each iteration. Affects the
 *                     parents and children.
 * @param endOfGenerationCondition A condition that will end the simulation early, such as
 *                                 solution convergence or reduction in population variance. Takes
 *                                 as parameters:
 *                                 - the iteration number
 *                                 - The current population
 *                                 - the scores of the current population
 *                                 Returns
 *                                 - true, if simulation should continue.
 *                                 - false, if the simulation should end.
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
 * @param generationOutputPrinter function that outputs information at each iteration. Takes as
 *                                parameters:
 *                                - iteration number
 *                                - population at start of iteration
 *                                - the scores for each member of the population
 *                                - the global best for all generations
 *                                - the score of the global best
 *                                - the local best (from the the current population)
 *                                - the score of the local best
 *                                Returns Unit.
 * @param scorer function that evaluates a member of population. Takes as parameters:
 *               - the solution to evaluate
 *               Returns a Double representing the score. A higher score is the most positive value.
 * @tparam T conforms to the type of TraitSeq
 *
 * @author Derek Hawker
 */
class GeneticAlgorithm[T](var population: Array[TraitSeq[T]],
                          val numGenerations: Int,
                          val mutationRate: Double,
                          endOfGenerationCondition: (Int, Array[TraitSeq[T]], Array[Double]) => Boolean,
                          fitPopulation: (Array[TraitSeq[T]], Array[Double]) => Array[Array[TraitSeq[T]]],
                          makeBabies: (Array[TraitSeq[T]]) => Array[TraitSeq[T]],
                          generationOutputPrinter: (Int, Array[TraitSeq[T]], Array[Double], TraitSeq[T], Double, TraitSeq[T], Double) => Unit,
                          scorer: TraitSeq[T] => Double)
  extends ExecutableAlgorithm[T] with Serializable{

  var currentGeneration = 0
  val filename = "ga.ser"

  /* must have an even number of parents.
   98 = bad because mating population is 49. 1 leftover parent.
   100 = good because  mating population is 50
   */
  assert(population.length % 4 == 0)
  assert(mutationRate >= 0.0 && mutationRate <= 1.0)


  /**
   * Run GA simulation based on population and other metrics given when creating new GA object.
   *
   * @return
   */
  def execute(): TraitSeq[T] = {
    val res = innerExecute()

    population = res._1 // Save the last evolved population

    // Return the very highestScoring trait sequence
    val globalBest = res._2
    globalBest._1
  }


  /**
   *
   * @return
   */
  private def innerExecute(): (Array[TraitSeq[T]], (TraitSeq[T], Double), (TraitSeq[T], Double)) = {
    (currentGeneration until (currentGeneration + numGenerations))
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

          currentGeneration += 1

          // Score all population
          val scores = pop.par.map(scorer).toArray
          val genBest = pop.zip(scores)
            .sortWith(_._2 > _._2)(0)
          val genBestScore = genBest._2
          val genBestTrait = genBest._1
          generationOutputPrinter(g, pop, scores, globalBest, globalBestScore, genBestTrait, genBestScore)


          /** **************************************************************************************
            * Early exit if meeting certain conditions.
            * This appears halfway in execution because a new iteration can't be made without
            * first scoring the babies made from last iteration. And it makes no sense to make a
            * new iteration, not score them, and THEN exit.
            */
          val canContinue = endOfGenerationCondition(g, pop, scores)
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


  /**
   * Mutates the given trait sequence
   *
   * @param ts
   * @return mutated copy of original
   */
  private def mutate(ts: TraitSeq[T]): TraitSeq[T] = {
    val cloned = ts.deepcopy()

    if (Random.nextDouble() < mutationRate) {
      val mutatedIndex = Random.nextInt(cloned.length)
      cloned(mutatedIndex) = cloned.randNeighbourhoodMove(mutatedIndex)
    }

    cloned
  }
}