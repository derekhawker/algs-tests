package com.derek.algs

import com.derek.algs.structures.{FiniteNeighbourhoodTraitSequence, TraitSequence, TraitSequenceVal}
import scala.util.Random
import scala.collection.mutable

/**
 * @author Derek Hawker
 */
class GeneticAlgorithm[T](val population: Array[TraitSequence[T]],
                          val numGenerations: Int,
                          val mutationRate: Double,
                          rng: Random,
                          endOfGenerationCondition: (Int, Array[TraitSequence[T]], Array[Double]) => Boolean,
                          scorer: TraitSequence[T] => Double) {

  /* must have an even number of parents.
   98 = bad because mating population is 49. 100 = good because  mating population is 50
   */
  assert(population.length % 4 == 0)
  assert(mutationRate >= 0.0 && mutationRate <= 1.0)

  private def findParents(traits: Array[TraitSequence[T]],
                          scores: Array[Double]): Array[Array[TraitSequence[T]]] = {

    val sorted = traits.zip(scores).sortWith(_._2 > _._2)

    val matingPop = sorted.map(_._1)
      .slice(0, sorted.length / 2)
    Array.range(0, matingPop.length, 2).map(
      i =>
        Array(matingPop(i), matingPop(i + 1))
    )
  }

  private def babyMaker(parents: Array[TraitSequence[T]]): Array[TraitSequence[T]] = {
    val (p1, p2) = (parents(0), parents(1))
    assert(p1.length == p2.length) // Condition that they both have same length

    val splitPoint = rng.nextInt(p1.length) + 1

    val child1 = p1.deepcopy()
    val child2 = p2.deepcopy()

    (0 until splitPoint).foreach(
      i =>
        child2(i) = child1(i))
    (splitPoint until p1.length).foreach(
      i =>
        child1(i) = child2(i))

    Array(child1, child2)
  }

  private def mutate(ts: TraitSequence[T]): TraitSequence[T] = {
    val cloned = ts.deepcopy()

    if (rng.nextDouble() < mutationRate) {
      val mutatedIndex = rng.nextInt(cloned.length)
      cloned(mutatedIndex) = cloned.randNeighbourhoodMove(mutatedIndex, rng)
    }

    cloned
  }


  def run(): TraitSequence[T] = {

    val best = innerRun()

    val pop = best._1
    val lastGen = best._2

    // Return the very best trait sequence
    lastGen._1
  }

  private def innerRun(): (Array[TraitSequence[T]], (TraitSequence[T], Double)) = {
    (0 until numGenerations).foldLeft(population,
      (population(0), Double.NegativeInfinity))(
        (d, g) => {

          val pop = d._1
          val lastGen = d._2
          val lastGenBest = lastGen._1
          val lastGenBestScore = lastGen._2

          // Score all GA
          val scores = pop.map(scorer)
          val genBest = pop.zip(scores).sortWith(_._2 > _._2)(0)

          println("^generation: " + g)

          val mean: Double = scores.sum / scores.length
          println("\tmean: " + mean
            + ", std.dev: " + math.sqrt(scores.foldLeft(0.0)(
            (count, s) =>
              count + math.pow(s - mean, 2)) / scores.length))

          println("\tBest: (%f) %s".format(genBest._2, genBest._1))


          /** **************************************************************************************
           Early exit if meeting certain conditions
            */
          val canContinue = endOfGenerationCondition(g, pop, scores)
          if (!canContinue)
            return (pop, if (genBest._2 > lastGenBestScore)
                           genBest
                         else
                           lastGen)
          /** **************************************************************************************
            */


          // Apply selection method to find breeding population
          val breedingPop = findParents(pop, scores)
          // Make babies
          val children = breedingPop.map(babyMaker).flatten

          // Mutate them
          val newpop = breedingPop.flatten ++ children
          val finalpop = newpop.map(mutate)

          (finalpop, if (genBest._2 > lastGenBestScore)
                       genBest
                     else
                       lastGen)
        })
  }

}
