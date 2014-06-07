package meta_heuristics.genetic_algorithms

import meta_heuristics.structures.specification.TraitSeq
import scala.util.Random
import meta_heuristics.GeneticAlgorithm

/**
 * @author Derek Hawker
 */
trait RandomMutation[T] extends GeneticAlgorithm[T]
{
   def mutate(ts: TraitSeq[T]): TraitSeq[T] =
   {
      val cloned = ts.deepcopy()

      if (Random.nextDouble() < mutationRate) {
         val mutatedIndex = Random.nextInt(cloned.length)
         cloned(mutatedIndex) = cloned.randNeighbourhoodMove(mutatedIndex)
      }

      cloned
   }
}
