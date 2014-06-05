package optimization

import meta_heuristics.util.ExecutableAlgorithm
import meta_heuristics.structures.specification.TraitSeq

/**
 * @author Derek Hawker
 */
class BranchAndBound[T] extends ExecutableAlgorithm[T] with Serializable
{


  override def execute(): TraitSeq[T] ={

    // iterate over collection in user specified way.

    throw new RuntimeException("not implemented")
  }
}

object BranchAndBound
{

}