package meta_heuristics.scoring

import meta_heuristics.structures.specification.TraitSeq

/**
 * @author Derek Hawker
 */
trait Rastrigin
{
  /**
   * @param traitsequence
   * @return
   */
  def scorer(traitsequence: TraitSeq[Double]): Double =
    -(10 * traitsequence.length
      + traitsequence.foldLeft(0.0)(
      (count,
       d) => {
        count + math.pow(d, 2) - 10 * math.cos(2 * math.Pi * d)
      }))
}
