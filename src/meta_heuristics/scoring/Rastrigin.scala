package meta_heuristics.scoring

import meta_heuristics.structures.specification.TraitSeq

/**
 * @author Derek Hawker
 */
trait Rastrigin
{
   /**
    * @param ts
    * @return
    */
   def traitScore(ts: TraitSeq[Double]): Double =
      -(10 * ts.length
         + ts.foldLeft(0.0)(
         (count,
          d) => {
            count + math.pow(d, 2) - 10 * math.cos(2 * math.Pi * d)
         }))
}
