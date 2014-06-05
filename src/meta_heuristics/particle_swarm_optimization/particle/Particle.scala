package meta_heuristics.particle_swarm_optimization.particle

import meta_heuristics.structures.specification.TraitSeq

/**
 *
 * @author Derek Hawker
 *
 * @param position
 * @param velocity
 * @tparam T
 */
class Particle[T](val position: TraitSeq[T],
                  val velocity: TraitSeq[T],
                  val localBest: TraitSeq[T],
                  val topology: Set[Particle[T]]) extends Iterable[(T, T, T)] with Serializable
{

   override def iterator: Iterator[(T, T, T)] = new Iterator[(T, T, T)]
   {
      var i = 0

      override def next(): (T, T, T) =
      {
         val index = i
         i += 1

         (position(index), velocity(index), localBest(index))
      }

      override def hasNext: Boolean =
         i < position.length
   }


   /**
    * Return a deep copy of the particle. Localbest is returned as a shallow copy because it
    * should already be a copy
    * @return deep copy of this particle
    */
   def deepcopy(): Particle[T] =

      new Particle[T](position.deepcopy(), velocity.deepcopy(), localBest, null)
}