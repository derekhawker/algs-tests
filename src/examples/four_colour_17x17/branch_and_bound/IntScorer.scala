package examples.four_colour_17x17.branch_and_bound

import meta_heuristics.structures.specification.TraitSeq
import examples.four_colour_17x17.Main

/**
 * Find the number of rectangles in 17x17 image. Rectangles here means the corner pieces with
 * the same colour.
 *
 * @author Derek Hawker
 */
trait IntScorer extends examples.four_colour_17x17.IntScorer
{
   private var indexMap = buildIndex

   private def buildIndex: Array[Int] =
   {
      val indexesUsed = Array.range(0, Main.width * Main.height).map(i => false)
      val indexMapper = Array.range(0, Main.width * Main.height).map(i => -1)

      var indexUnused: Int  = 0
      (1 to math.max(Main.width, Main.height)).foreach(width => {
         (0 until math.min(width, Main.height)).foreach(w => {
            (0 until math.min(width, Main.width)).foreach(h => {
               val ind = w * Main.width + h
               if (!indexesUsed(ind)) {
                  indexesUsed(ind) = true
                  indexMapper(indexUnused) = ind
                  indexUnused += 1
               }
            })
         })
      })
      assert(indexUnused == Main.numFeatures)

      indexMapper
   }

   final override def traitScore(ts: TraitSeq[Int]): Double =
   {
      val copyts = ts.deepcopy()
      (0 until ts.length).foreach(i =>
         copyts(indexMap(i)) = ts(i))

      super.traitScore(copyts)
   }
}
