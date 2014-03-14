package com.derek.algs.structures


/**
 * @author Derek Hawker
 */
class TraitSeqRef[T <: AnyRef](override val xs: Array[T],
                                    override val neighbourhood: Array[Array[T]],
                                    createDeepCopyRef: T => T)
  extends FiniteNeighbourhoodTraitSeq[T](xs, neighbourhood) {

  /**
   * Ensure that the clone method of your Reference type T implements a deep copy
   *
   * @return Deep copy of this trait
   */
  override def deepcopy(): TraitSeqRef[T] = {
    val deepArray = xs.slice(0, xs.length)
    deepArray.zipWithIndex.foreach(r =>deepArray(r._2) = createDeepCopyRef(r._1))

    new TraitSeqRef(deepArray, neighbourhood, createDeepCopyRef)
  }
}


/**
 * @author Derek Hawker
 */
object TraitSeqRef {
  def main(args: Array[String]) {
    val h = new TraitSeqRef[(Int, Char)](Array((2, 'c'), (1, 'b')),
      Array(Array((1, 'b'), (2, 'c'), (3, 'a')),
        Array((1, 'b'), (2, 'c'), (3, 'a'))), (t: (Int, Char)) => (t._1, t._2))
    val g = h.deepcopy()

    g(0) = g.neighbourhood(0)(2)
    println(g)
    println(h)
  }
}