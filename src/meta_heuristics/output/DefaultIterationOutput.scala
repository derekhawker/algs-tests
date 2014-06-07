package meta_heuristics.output

import meta_heuristics.structures.specification.TraitSeq

/**
 * @author Derek Hawker
 */
trait DefaultIterationOutput[T]
{
   def printIteration(iteration: Int,
                      population: Array[TraitSeq[T]],
                      scores: Array[Double],
                      globalBest: TraitSeq[T],
                      globalBestScore: Double,
                      localBest: TraitSeq[T],
                      localBestScore: Double)
   {

      println("^iteration: " + iteration)

      val mean: Double = scores.sum / scores.length
      println("\tmean: " + mean
         + ", std.dev: " + math.sqrt(scores.foldLeft(0.0)(
         (count, s) =>
            count + math.pow(s - mean, 2.0)) / scores.length))

      println("\t[Global]Best: score = %f %s".format(globalBestScore, globalBest))
      println("\t[Local]Best:  score = %f %s".format(localBestScore, localBest))
   }
}





