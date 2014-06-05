package meta_heuristics.scoring

import meta_heuristics.structures.specification.TraitSeq

/**
 * @author Derek Hawker
 */
trait Rosenbrock
{

  /**
   * @param traitsequence
   * @return
   */
  def scorer(traitsequence: TraitSeq[Double]): Double =
    (0 until traitsequence.length - 1)
      .foldLeft(0.0)(
        (count,
         i) => {
          (count
            + 100 * math.pow(traitsequence(i + 1)
            - math.pow(traitsequence(i), 2)
            , 2)
            + math.pow(traitsequence(i) - 1, 2))
        })
}

