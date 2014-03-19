package com.derek.algs

import scala.util.Random
import com.derek.algs.structures.specification.TraitSeq
import com.derek.algs.util.ExecutableAlgorithm


/**
 * @author Derek Hawker
 */
object GeneticAlgorithm {

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
     * @param parents
     * @return
     */
    def spliceParents[T](parents: Array[TraitSeq[T]]): Array[TraitSeq[T]] = {
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
 * @author Derek Hawker
 */
class GeneticAlgorithm[T](val population: Array[TraitSeq[T]],
                          val numGenerations: Int,
                          val mutationRate: Double,
                          endOfGenerationCondition: (Int, Array[TraitSeq[T]], Array[Double]) => Boolean,
                          findParents: (Array[TraitSeq[T]], Array[Double]) => Array[Array[TraitSeq[T]]],
                          makeBabies: (Array[TraitSeq[T]]) => Array[TraitSeq[T]],
                          generationOutputPrinter: (Int, Array[TraitSeq[T]], Array[Double], (TraitSeq[T], Double), (TraitSeq[T], Double)) => Unit,
                          scorer: TraitSeq[T] => Double) extends ExecutableAlgorithm[T] {

  /* must have an even number of parents.
   98 = bad because mating population is 49. 1 leftover parent.
   100 = good because  mating population is 50
   */
  assert(population.length % 4 == 0)
  assert(mutationRate >= 0.0 && mutationRate <= 1.0)

  def execute(): TraitSeq[T] = {

    val best = innerExecute()

    val pop = best._1
    val globalBest = best._2

    // Return the very highestScoringParticle trait sequence
    globalBest._1
  }


  private def innerExecute(): (Array[TraitSeq[T]], (TraitSeq[T], Double), (TraitSeq[T], Double)) = {
    (0 until numGenerations)
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

          // Score all GA
          val scores = pop.par.map(scorer).toArray
          val genBest = pop.zip(scores)
            .sortWith(_._2 > _._2)(0)
          generationOutputPrinter(g, pop, scores, global, genBest)


          /** **************************************************************************************
            * Early exit if meeting certain conditions.
            * This appears halfway in execution because a new generation can't be made without
            * first scoring the babies made from last generation. And it makes no sense to make a
            * new generation and only THEN exit.
            */
          val canContinue = endOfGenerationCondition(g, pop, scores)
          if (!canContinue)
            return (pop, if (genBest._2 > globalBestScore)
                           genBest
                         else
                           global,
              genBest)

          /** *************************************************************************************/


          // Apply selection method to find breeding population
          val breedingPop = findParents(pop, scores)
          // Make babies
          val children = breedingPop.par.map(makeBabies).flatten.toArray
          // Mutate them
          val newpop = breedingPop.flatten ++ children
          val finalpop = newpop.map(mutate)

          (finalpop, if (genBest._2 > globalBestScore)
                       genBest
                     else
                       global,
            genBest)
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


