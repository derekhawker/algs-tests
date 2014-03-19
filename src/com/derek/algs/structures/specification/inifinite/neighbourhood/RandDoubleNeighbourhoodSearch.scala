package com.derek.algs.structures.specification.inifinite.neighbourhood

import com.derek.algs.structures.specification.TraitSeq
import scala.util.Random

/**
 * @author Derek Hawker
 */
trait RandDoubleNeighbourhoodSearch extends TraitSeq[Double] {

  /**
   *
   * @return _1: lower bound, _2: upper bound
   */
  def ranges: Array[(Double, Double)]

  val maxTries = 3

  def randNeighbourhoodMove(move: Int): Double = {
    Random.nextDouble()*(ranges(move)._2 - ranges(move)._1)+ ranges(move)._1
  }

  def bestNeighbourhoodMove(move: Int,
                            scorer: (TraitSeq[Double]) => Double): (TraitSeq[Double], Double) =
    (0 until maxTries)
      .foldLeft((this.asInstanceOf[TraitSeq[Double]], Double.NegativeInfinity))(
        (best, i) => {
          val newTs = this.deepcopy()
          newTs(move) = randNeighbourhoodMove(move)
          val newscore = scorer(newTs)

          if (newscore > best._2)
            (newTs, newscore)
          else
            best
        })
}
