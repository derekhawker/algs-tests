package com.derek.algs.structures

import scala.util.Random

/**
 * @author Derek Hawker
 */
abstract class TraitSequenceFiniteNeighbourhood[T](val xs: Array[T],
                                                   val neighbourhood: Array[Array[T]]) extends Iterable[T]
with TraitSequence[T] {


  override def iterator: Iterator[T] = xs.iterator

  def length = xs.length

  def apply(index: Int): T =
    xs(index)

  def update(index: Int, value: T): Unit =
    xs(index) = value


  override def bestNeighbourhoodMove(move: Int,
                                     scorer: (TraitSequence[T]) => Double): (TraitSequence[T], Double) = {

    neighbourhood(move).foldLeft((this, Double.NegativeInfinity))(
      (best, tr) => {
        val newSolution = deepcopy().asInstanceOf[TraitSequenceFiniteNeighbourhood[T]]
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


class TraitSequenceVal[T <: AnyVal](override val xs: Array[T],
                                    override val neighbourhood: Array[Array[T]])
  extends TraitSequenceFiniteNeighbourhood[T](xs, neighbourhood) {

  override def deepcopy(): TraitSequenceVal[T] =
    new TraitSequenceVal[T](xs.clone(), neighbourhood)

}

//class TraitSequenceRef[T <: AnyRef with CopyRef[T]](override val xs: Array[T],
//                                    override val neighbourhood: Array[Array[T]])
//  extends TraitSequenceFiniteNeighbourhood[T](xs, neighbourhood) {
//
//  /**
//   * Ensure that the clone method of your Reference type T implements a deep copy
//   *
//   * @return Deep copy of this trait
//   */
//  override def deepcopy(): TraitSequenceRef[T] = {
//    val deepArray = xs.map(r => r.copy)
//    new TraitSequenceRef(deepArray.array, neighbourhood)
//  }
//}

trait CopyRef[T] {
  def copy: T
}
//class FeaturesHyperplane(weights: Array[(Boolean, Double)]) extends TraitSequence(weights)