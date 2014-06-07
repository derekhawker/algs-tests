package examples.hello_world

import meta_heuristics.structures.specification.TraitSeq
import meta_heuristics.structures.concrete.finite.neighbourhood.TraitSeqRef

/**
 * @author Derek Hawker
 */
trait HelloWorldStringScorer
{
    def traitScore(traitsequence: TraitSeq[String]): Double =
    {
       val ts = traitsequence.asInstanceOf[TraitSeqRef[String]]

       "HelloWorld".zip(ts).foldLeft(0.0)(
          (score, zipped) => {
             val perfectChar = zipped._1
             val tsChar = zipped._2

             score - math.abs(perfectChar - tsChar.toCharArray()(0))
          }
       )
    }
 }
