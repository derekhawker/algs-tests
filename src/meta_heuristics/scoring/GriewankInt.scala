package meta_heuristics.scoring

import meta_heuristics.structures.specification.TraitSeq

/**
 * @author Derek Hawker
 */
trait GriewankInt
{
   final def traitScore(ts: TraitSeq[Int]): Double =
      -(1 +
         (ts.foldLeft(0.0)(
            (count,
             d) =>
               count + math.pow(d, 2)) / 4000)
         - ts.zipWithIndex
         .foldLeft(1.0)(
            (count,
             pair) => {
               val i = pair._2
               count * math.cos(pair._1 / math.sqrt(i + 1))
            }
         ))
}
