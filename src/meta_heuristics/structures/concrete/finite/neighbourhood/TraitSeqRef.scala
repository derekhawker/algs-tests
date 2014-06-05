package meta_heuristics.structures.concrete.finite.neighbourhood

import meta_heuristics.structures.specification.finite.neighbourhood.FiniteNeighbourhoodTraitSeq

/**
 * A TraitSeq for Reference types in the TraitSeq array.
 * @author Derek Hawker
 *
 * @param xs represents the trait sequence
 * @param neighbourhood
 * @param createDeepCopyRef a function to build a completely deep copy of the reference value
 *                          stored in the trait array.
 * @tparam T
 */
class TraitSeqRef[T <: AnyRef](override val xs: Array[T],
                               override val neighbourhood: Array[Array[T]],
                               createDeepCopyRef: T => T)
   extends FiniteNeighbourhoodTraitSeq[T](xs, neighbourhood)
{

   /**
    *
    * @return Deep copy of this trait (all levels)
    */
   override def deepcopy(): TraitSeqRef[T] =
   {
      // Lots of problems creating a new array that doesn't become an ArraySeq. This works for now
      val deepArray = xs.slice(0, xs.length)
      deepArray.zipWithIndex
         .foreach(r =>
         deepArray(r._2) = createDeepCopyRef(r._1))

      new TraitSeqRef(deepArray, neighbourhood, createDeepCopyRef)
   }
}


/**
 * @author Derek Hawker
 */
object TraitSeqRef
{
   def main(args: Array[String])
   {
      val h = new TraitSeqRef[(Int, Char)](Array((2, 'c'), (1, 'b')),
         Array(Array((1, 'b'), (2, 'c'), (3, 'a')),
            Array((1, 'b'), (2, 'c'), (3, 'a'))),
         (t: (Int, Char)) => (t._1, t._2))

      val g = h.deepcopy()

      g(0) = g.neighbourhood(0)(2)
      println(g)
      println(h)
   }
}