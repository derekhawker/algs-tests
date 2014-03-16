package com.derek.algs.structures.inifinite.neighbourhood

import com.derek.algs.structures.specification.TraitSeq
import scala.util.Random

/**
 * @author Derek Hawker
 */
trait RandIntNeighbourhoodSearch extends TraitSeq[Int] {

  /**
   * _1: lower bound,
   * _2: upper bound
   * @return
   */
  def ranges: Array[(Int, Int)]

  val maxTries = 3

  def randNeighbourhoodMove(move: Int): Int = {
    Random.nextInt(ranges(move)._2 - ranges(move)._1)+ ranges(move)._1
  }

  def bestNeighbourhoodMove(move: Int,
                            scorer: (TraitSeq[Int]) => Double): (TraitSeq[Int], Double) =
    (0 until maxTries)
      .foldLeft((this.asInstanceOf[TraitSeq[Int]], Double.NegativeInfinity))(
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
