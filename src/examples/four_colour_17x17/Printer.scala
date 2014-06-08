package examples.four_colour_17x17

import meta_heuristics.structures.specification.TraitSeq
import meta_heuristics.util.gif.AnimatedProgressGif
import meta_heuristics.output.DefaultIterationOutput

/**
 * @author Derek Hawker
 */
trait Printer extends DefaultIterationOutput[Int]
{

   final override def printIteration(iteration: Int,
                               population: Array[TraitSeq[Int]],
                               scores: Array[Double],
                               globalBest: TraitSeq[Int],
                               globalBestScore: Double,
                               localBest: TraitSeq[Int],
                               localBestScore: Double)
   {

      super.printIteration(iteration, population, scores, globalBest, globalBestScore,
         localBest, localBestScore)

      AnimatedProgressGif.apply.
         addFrame(globalBest.asInstanceOf[TraitSeq[Int]])
   }
}


