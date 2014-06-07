package meta_heuristics.structures.concrete.finite.neighbourhood

import meta_heuristics.structures.specification.TraitSeq
import scala.util.Random

class TraitSeqVal[@specialized(Int, Double) T](xs: Array[T],
                               neighbourhood: Array[Array[T]])
   extends TraitSeq[T] with Serializable
{


   override def iterator: Iterator[T] = xs.iterator

   def length = xs.length

   def apply(index: Int): T = xs(index)

   def update(index: Int, value: T): Unit = xs(index) = value


   /**
    * At a particular slot in solution array, tries all possible replacement values and returns the
    * trait with the highest score
    *
    * @param move
    * @param scorer
    * @return
    */
   override def bestNeighbourhoodMove(move: Int,
                                      scorer: (TraitSeq[T]) => Double): (TraitSeq[T], Double) =
   {

      // Create a copy of trait, make the move to neighbouring solution. Evaluate.
      neighbourhood(move)
         .foldLeft((this, Double.NegativeInfinity))(
            (best, tr) => {

               val newSolution = deepcopy()
               newSolution(move) = tr
               val score = scorer(newSolution)

               if (score > best._2)
                  (newSolution, score)
               else
                  best
            })
   }

   override def randNeighbourhoodMove(move: Int): T =
   {
      val numMoves = neighbourhood(move).length

      neighbourhood(move)(Random.nextInt(numMoves))
   }

   override def toString(): String =
      "Sol'n: " + xs.mkString("\"", ",", "\"")

   override def deepcopy(): TraitSeqVal[T] =
      new TraitSeqVal[T](xs.clone(), neighbourhood)

}