package meta_heuristics.genetic_algorithms.population_selector

import meta_heuristics.structures.specification.TraitSeq

/**
 * @author Derek Hawker
 */
trait EliteSelection[T]
{
  def fitPopulation(traits: Array[TraitSeq[T]],
                    scores: Array[Double]): Array[Array[TraitSeq[T]]] =
  {

    val sorted = traits.zip(scores).sortWith(_._2 > _._2)

    val matingPop = sorted.map(_._1)
      .slice(0, sorted.length / 2)

    Array.range(0, matingPop.length, 2)
      .map(i =>
      Array(matingPop(i), matingPop(i + 1))
      )
  }
}
