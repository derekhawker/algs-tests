package com.derek.algs.structures

import scala.util.Random

/**
 * @author Derek Hawker
 */
abstract class FiniteNeighbourhoodTraitSeq[T](val xs: Array[T],
                                                   val neighbourhood: Array[Array[T]]) extends Iterable[T]
with TraitSeq[T] {


  override def iterator: Iterator[T] = xs.iterator

  def length = xs.length

  def apply(index: Int): T =
    xs(index)

  def update(index: Int, value: T): Unit =
    xs(index) = value


  override def bestNeighbourhoodMove(move: Int,
                                     scorer: (TraitSeq[T]) => Double): (TraitSeq[T], Double) = {

    neighbourhood(move).foldLeft((this, Double.NegativeInfinity))(
      (best, tr) => {
        val newSolution = deepcopy().asInstanceOf[FiniteNeighbourhoodTraitSeq[T]]
        newSolution(move) = tr
        val score = scorer(newSolution)

        if (score > best._2)
          (newSolution, score)
        else
          best
      })
  }

  override def randNeighbourhoodMove(move: Int, rng: Random): T = {
    val numMoves = neighbourhood(move).length

    neighbourhood(move)(rng.nextInt(numMoves))
  }

  override def toString(): String =
    "TS: " + xs.mkString("[", "", "]")
}
