package meta_heuristics.structures.specification


/**
 * @author Derek Hawker
 */
trait TraitSeq[@specialized(Char, Double, Int) T] extends Iterable[T]
{
   def bestNeighbourhoodMove(move: Int,
                             scorer: (TraitSeq[T]) => Double): (TraitSeq[T], Double)

   def randNeighbourhoodMove(move: Int): T

   def deepcopy(): TraitSeq[T]

   def length: Int

   def apply(index: Int): T

   def update(index: Int, value: T): Unit
}

