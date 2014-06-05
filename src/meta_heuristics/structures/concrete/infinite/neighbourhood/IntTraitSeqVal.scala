package meta_heuristics.structures.concrete.infinite.neighbourhood

import meta_heuristics.structures.specification.inifinite.neighbourhood.{RandDoubleNeighbourhoodSearch, InfiniteNeighbourhoodTraitSeq, RandIntNeighbourhoodSearch}

/**
 *
 * @param xs
 * @param ranges
 *
 * @author Derek Hawker
 */
class IntTraitSeqVal(override val xs: Array[Int],
                     val ranges: Array[(Int, Int)])
   extends InfiniteNeighbourhoodTraitSeq[Int](xs) with RandIntNeighbourhoodSearch
{

   override def deepcopy(): IntTraitSeqVal =
      new IntTraitSeqVal(xs.clone(), ranges)
      {
      }
}


/**
 *
 * @param xs
 * @param ranges
 *
 * @author Derek Hawker
 */
class DoubleTraitSeqVal(override val xs: Array[Double],
                        val ranges: Array[(Double, Double)])
   extends InfiniteNeighbourhoodTraitSeq[Double](xs) with RandDoubleNeighbourhoodSearch
{

   override def deepcopy(): DoubleTraitSeqVal =
      new DoubleTraitSeqVal(xs.clone(), ranges)
      {
      }

   override def toString(): String =
   {
      xs.foldLeft(new StringBuilder("["))(
         (sb, x) =>
            sb.append("%.03f".format(x)).append(","))
         .append("]")
         .toString()
   }
}
