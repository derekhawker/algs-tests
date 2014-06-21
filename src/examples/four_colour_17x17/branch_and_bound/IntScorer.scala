package examples.four_colour_17x17.branch_and_bound

import meta_heuristics.structures.specification.TraitSeq
import examples.four_colour_17x17.Main
import com.typesafe.scalalogging.slf4j.StrictLogging


/**
 * Find the number of rectangles in 17x17 image. Rectangles here means the corner pieces with
 * the same colour.
 *
 * @author Derek Hawker
 */
trait IntScorer extends examples.four_colour_17x17.IntScorerOptimized with StrictLogging
{
   private final val indexMap = buildIndexTwoByTwo

   private def buildIndexOutToIn: Array[Int] =
   {
      val indexesUsed = Array.range(0, Main.width * Main.height).map(i => false)
      val indexMapper = Array.range(0, Main.width * Main.height).map(i => -1)

      var indexUnused: Int = 0
      ((math.max(Main.width, Main.height)) to 0 by -1).foreach(width => {
         ((math.min(width, Main.height) - 1) to 0 by -1).foreach(w => {
            ((math.min(width, Main.width) - 1) to 0 by -1).foreach(h => {
               val ind = w * Main.width + h
               if (!indexesUsed(ind)) {
                  indexesUsed(ind) = true
                  indexMapper(indexUnused) = ind
                  indexUnused += 1
               }
            })
         })
      })

      logger.debug("indexesUsed: " + indexUnused)
      assert(indexUnused == Main.numFeatures)

      indexMapper
   }

   private def buildIndexTwoByTwo: Array[Int] =
   {
      val indexesUsed = Array.range(0, Main.width * Main.height).map(i => false)
      val indexMapper = Array.range(0, Main.width * Main.height).map(i => -1)

      var indexUnused: Int = 0
      (0 until Main.height by 2).foreach(row => {
         (0 until Main.width).foreach(col => {

            (row until math.min(Main.height, row + 2)).foreach(r => {
               val ind = r * Main.width + col
               if (!indexesUsed(ind)) {
                  indexesUsed(ind) = true
                  indexMapper(indexUnused) = ind
                  indexUnused += 1
               }
            })
         })
      })

      logger.debug("indexesUsed: " + indexUnused)
      assert(indexUnused == Main.numFeatures)

      indexMapper
   }

   final override def traitScore(ts: TraitSeq[Int]): Double =
   {
      // Remapped to encourage early recognition of colouring violations.
      val copyts = ts.deepcopy()
      (0 until ts.length).foreach(i =>
         copyts(indexMap(i)) = ts(i))

      super.traitScore(copyts)
   }

   final def boundedTraitScore(ts: TraitSeq[Int], level: Int): Double =
      traitScore(ts)
}
