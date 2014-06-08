package examples.hello_world

import meta_heuristics.structures.specification.TraitSeq
import meta_heuristics.structures.concrete.finite.neighbourhood.TraitSeqVal

/**
 * @author Derek Hawker
 */
trait HelloWorldCharScorer
{
    final def traitScore(traitsequence: TraitSeq[Char]): Double =
    {
       val ts = traitsequence.asInstanceOf[TraitSeqVal[Char]]

       "HelloWorld".zip(ts).foldLeft(0.0)(
          (score, zipped) => {
             val perfectChar = zipped._1
             val tsChar = zipped._2

             score - math.abs(perfectChar - tsChar)
          }
       )
    }
 }
