package meta_heuristics.scoring

import meta_heuristics.structures.specification.TraitSeq

/**
 * @author Derek Hawker
 */
trait Rosenbrock
{
   /**
    * @param ts
    * @return
    */
   def traitScore(ts: TraitSeq[Double]): Double =
      (0 until ts.length - 1)
         .foldLeft(0.0)(
            (count,
             i) => {
               (count
                  + 100 * math.pow(ts(i + 1)
                  - math.pow(ts(i), 2)
                  , 2)
                  + math.pow(ts(i) - 1, 2))
            })
}

