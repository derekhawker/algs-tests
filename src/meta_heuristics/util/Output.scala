package meta_heuristics.util

import meta_heuristics.structures.specification.TraitSeq
import meta_heuristics.structures.concrete.infinite.neighbourhood.{INTraitSeqVal, DoubleTraitSeqVal}
import meta_heuristics.particle.Particle

/**
 * @author Derek Hawker
 */
object Output {
  def defaultIterationPrinter[T](iteration: Int,
                        population: Array[TraitSeq[T]],
                        scores: Array[Double],
                        globalBest: TraitSeq[T],
                        globalBestScore: Double,
                        localBest: TraitSeq[T],
                        localBestScore: Double) {
    println("^iteration: " + iteration)

    val mean: Double = scores.sum / scores.length
    println("\tmean: " + mean
      + ", std.dev: " + math.sqrt(scores.foldLeft(0.0)(
      (count, s) =>
        count + math.pow(s - mean, 2.0)) / scores.length))

    println("\t[Global]Best: score = %f %s".format(globalBestScore, globalBest))
    println("\t[Local]Best:  score = %f %s".format(localBestScore, localBest))
  }


  def psoIterationPrinter[T](i: Int,
                             population: Array[Particle[T]],
                             scores: Array[Double],
                             globalBest: Particle[T],
                             globalBestScore: Double,
                             localBest: Particle[T],
                             localBestScore: Double) {
    println("^iteration: " + i)
    val mean: Double = scores.sum / scores.length
    println("\tmean: " + mean
      + ", std.dev: " + math.sqrt(scores.foldLeft(0.0)(
      (count, s) =>
        count + math.pow(s - mean, 2.0)) / scores.length))

    println("\t[Global]Best score = %f %s".format(globalBestScore, globalBest.position))
    println("\t[Local]Best  score = %f %s".format(localBestScore, localBest.position))

    /**
     * Nice to have for debugging.
     */
//    population
//      .foreach(p => {
//      println("\t" + p.position + " " + p.velocity + " score: " + Scoring.doubleColour17x17Scorer(
//        p.position.asInstanceOf[INTraitSeql[Double]]))
//    })
  }
}
