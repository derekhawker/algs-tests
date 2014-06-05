package meta_heuristics.output

import meta_heuristics.structures.specification.TraitSeq
import meta_heuristics.util.gif.AnimatedProgressGif

/**
 * @author Derek Hawker
 */
trait FourColour17x17Printer extends DefaultIterationOutput[Int]
{

  override def printIteration(iteration: Int,
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
