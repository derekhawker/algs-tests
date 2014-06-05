package meta_heuristics.scoring

import meta_heuristics.structures.specification.TraitSeq

/**
 * @author Derek Hawker
 */
trait GriewankDouble
{
   /**
    * @param traitsequence
    * @return
    */
   def scorer(traitsequence: TraitSeq[Double]): Double =
      -(1 +
         (traitsequence.foldLeft(0.0)(
            (count,
             d) =>
               count + math.pow(d, 2))
            / 4000)
         - traitsequence.zipWithIndex
         .foldLeft(1.0)(
            (count,
             pair) => {
               val i = pair._2
               count * math.cos(pair._1 / math.sqrt(i + 1))
            }
         ))
}
