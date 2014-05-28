package algs.meta_heuristics.particle.structures.specification.inifinite.neighbourhood

import algs.meta_heuristics.particle.structures.specification.TraitSeq

/**
 * @author Derek Hawker
 */
abstract class InfiniteNeighbourhoodTraitSeq[T](val xs: Array[T]) extends TraitSeq[T] {

  override def iterator: Iterator[T] = xs.iterator

  def length = xs.length

  def apply(index: Int): T =
    xs(index)

  def update(index: Int, value: T): Unit =
    xs(index) = value

  override def toString(): String =
    "TS: " + xs.mkString("[", ",", "]")

}
