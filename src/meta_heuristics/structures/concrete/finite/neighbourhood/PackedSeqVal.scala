package meta_heuristics.structures.concrete.finite.neighbourhood

import meta_heuristics.structures.specification.TraitSeq
import scala.util.Random

//
//import meta_heuristics.structures.specification.TraitSeq
//import scala.collection.mutable
//
///**
// * @author Derek Hawker
// */
//class PackedSeqVal(bitSize: Int,
//                   numItems: Int,
//                   xs: mutable.BitSet,
//                   neighbourhood: Array[Array[Int]]) extends TraitSeq[Int]
//{
//  assert(bitSize < 32 && (32 % bitSize) == 0)
//  private val BIT_FLAG = mutable.BitSet.fromBitMask(Array.range(0, bitSize).map(b => 1L)).elems(0)
//
//  override def deepcopy(): PackedSeqVal =
//    new PackedSeqVal(bitSize, numItems, xs.clone(), neighbourhood)
//
//  override def bestNeighbourhoodMove(move: Int,
//                                     scorer: (TraitSeq[Int]) => Double): (TraitSeq[Int], Double) =
//    throw new RuntimeException("not implemented")
//
//  override def update(index: Int, value: Int): Unit =
//  {
//    new PackedInt
//  }
//
//  override def length: Int =
//    numItems
//
//  override def randNeighbourhoodMove(move: Int): Int =
//    throw new RuntimeException("not implemented")
//
//
//  // 4 bits, 2 index ()
//  // 2*4 = 8 / 64 = 0
//
//  override def apply(index: Int): Int =
//  {
//    val bitIndex: Int = index * bitSize
//    ((xs.elems(bitIndex / 64)) >> bitIndex) & BIT_FLAG).asInstanceOf[Int]
//  }
//
//  override def iterator: Iterator[Int] =
//    new Iterator[Int]()
//    {
//      var i: Int = 0
//
//      override def hasNext: Boolean =
//        i < numItems
//
//      override def next(): Int =
//      {
//        val elem = apply(i)
//        i += 1
//
//        elem
//      }
//    }
//}
//
//
//


class PackedSeqVal(bitSize: Int,
                   numItems: Int,
                   xs: Array[Int],
                   neighbourhood: Array[Array[Int]]) extends TraitSeq[Int]
{
  assert(bitSize < 32 && (32 % bitSize) == 0)
  private val BIT_FLAG = (0 until bitSize).foldLeft(0)((res, b) => (res << 1) + 1)

  override def bestNeighbourhoodMove(move: Int,
                                     scorer: (TraitSeq[Int]) => Double): (TraitSeq[Int], Double) =
  {
    neighbourhood(move)
      .foldLeft((this.asInstanceOf[TraitSeq[Int]], Double.NegativeInfinity))(
        (best, tr) =>
        {

          val newSolution = deepcopy()
          newSolution(move) = tr
          val score = scorer(newSolution)

          if (score > best._2)
            (newSolution, score)
          else
            best
        })
  }

  var packedSize = 32/bitSize

  var cached   = apply(0)
  var maxIndex = packedSize
  var minIndex = 0

  override def update(index: Int, value: Int): Unit =
  {
    val bitIndex = index * bitSize

    xs(bitIndex / 32) |= ((value & BIT_FLAG) << bitIndex)

  }

  override def length: Int =
    numItems

  override def randNeighbourhoodMove(move: Int): Int = {
    val numMoves = neighbourhood(move).length

    neighbourhood(move)(Random.nextInt(numMoves))
  }

  override def apply(index: Int): Int =
  {
    if (index < maxIndex && index >= minIndex)
    {
      (cached >> (index%packedSize)) & BIT_FLAG
    } else
    {

      val bitIndex = index * bitSize

      minIndex = index - (index % packedSize)
      maxIndex = minIndex + packedSize
      cached = xs(bitIndex / 32)

      val elem = (cached >> (index % packedSize)) & BIT_FLAG
      elem
    }
  }

  override def deepcopy(): TraitSeq[Int] =
    new PackedSeqVal(bitSize, numItems, xs.clone(), neighbourhood)


  override def iterator: Iterator[Int] =
    new Iterator[Int]()
    {
      var index = 0

      override def hasNext: Boolean =
        index < numItems

      override def next(): Int =
      {
        val elem = apply(index)
        index += 1

        elem
      }
    }
}