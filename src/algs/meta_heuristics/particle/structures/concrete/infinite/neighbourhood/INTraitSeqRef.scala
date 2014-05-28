package algs.meta_heuristics.particle.structures.concrete.infinite.neighbourhood

import algs.meta_heuristics.particle.structures.specification.TraitSeq
import java.awt.Color

class INTraitSeqRef[T <: AnyRef](xs: Array[T],
                                 newDeepCopy: (T) => T)
  extends TraitSeq[T] with Serializable {

  override def iterator: Iterator[T] =
    xs.iterator

  override def update(index: Int,
                      value: T): Unit =
    xs(index) = value

  override def apply(index: Int): T =
    xs(index)

  override def length: Int =
    xs.length

  override def deepcopy(): TraitSeq[T] = {
    // Lots of problems creating a new array that doesn't become an ArraySeq. This works for now
    val deepArray = xs.slice(0, xs.length)

    deepArray.zipWithIndex
      .foreach(r =>
      deepArray(r._2) = newDeepCopy(r._1))

    new INTraitSeqRef(deepArray,newDeepCopy).asInstanceOf[TraitSeq[T]]
  }

  override def randNeighbourhoodMove(move: Int): T =
    throw new RuntimeException("Not implemented")

  override def bestNeighbourhoodMove(move: Int,
                                     scorer: (TraitSeq[T]) => Double): (TraitSeq[T], Double) =
    throw new RuntimeException("Not implemented")
}

