package optimization

import meta_heuristics.util.ExecutableAlgorithm
import meta_heuristics.structures.specification.TraitSeq
import scala.collection.mutable


/**
 * @author Derek Hawker
 */
class BranchAndBound[T](var currentSolution: TraitSeq[T])
   extends ExecutableAlgorithm[T] with Serializable
{

   type Incumbent = (TraitSeq[T], Double)

   val incumbents = new mutable.PriorityQueue[Incumbent]()(
      new Ordering[Incumbent]
      {
         override def compare(x: Incumbent, y: Incumbent): Int =
            (x._2 - y._2).toInt
      })

   override def execute(): TraitSeq[T] =
   {
      val results = innerExecute()

      results
   }

   /**
    *
    * @return (best solution, last solution)
    */
   def innerExecute(): TraitSeq[T] =
   {
      throw new RuntimeException("not implemented")
   }

   // iterate over collection in user specified way.

   // depth-first or bread-first? Arbitrary ordering?

}

object BranchAndBound
{


}