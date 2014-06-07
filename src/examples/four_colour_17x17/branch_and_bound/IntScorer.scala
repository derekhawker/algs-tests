package examples.four_colour_17x17.branch_and_bound

import meta_heuristics.structures.specification.TraitSeq

/**
 * Find the number of rectangles in 17x17 image. Rectangles here means the corner pieces with
 * the same colour.
 *
 * @author Derek Hawker
 */
trait IntScorer
{
   private var indexMap = buildIndex

   private def buildIndex: Array[Int] =
   {
      val indexesUsed = Array.range(0, 17 * 17).map(i => false)
      val indexMapper = Array.range(0, 17 * 17).map(i => -1)

      var indexUnused: Int  = 0
      (1 to 17).foreach(width => {
         (0 until width).foreach(w => {
            (0 until width).foreach(h => {
               val ind = w * 17 + h
               if (!indexesUsed(ind)) {
                  indexesUsed(ind) = true
                  indexMapper(indexUnused) = ind
                  indexUnused += 1
               }
            })
         })
      })
      assert(indexUnused == 17 * 17)

      indexMapper
   }

   def traitScore(ts: TraitSeq[Int]): Double =
   {
      val copyts = ts.deepcopy()
      (0 until ts.length).foreach(i =>
         copyts(indexMap(i)) = ts(i))

      val colouringViolations = (0 until (17 - 1)).foldLeft {0}(
         (totalViolations, row) =>
            totalViolations
               + (0 until (17 - 1)).foldLeft(0)(
               (colViolations, col) =>
                  colViolations + numSameColourRectangles(copyts, row, col)
            ))

      // The less violations, the higher the score
      -colouringViolations
   }

   /**
    * Helper function that searches all squares to the left and right from current square to find
    * squares that have the same colour and form the corner points of a rectangle.
    *
    * @param traitsequence
    * @param starty
    * @param startx
    * @return
    */
   private def numSameColourRectangles(traitsequence: TraitSeq[Int],
                                       starty: Int,
                                       startx: Int): Int =
   {
      val ul = starty * 17 + startx

      ((starty + 1) until 17).foldLeft(0)(
         (rowViolations, row) => {
            val bl = row * 17 + startx

            // Only continue if upper-left and bottom-left corners are same colour
            if (traitsequence(bl) != traitsequence(ul)) {
               rowViolations + 0
            } else {
               (rowViolations
                  + ((startx + 1) until 17).foldLeft(0)(
                  (colViolations, col) => {
                     val ur = starty * 17 + col
                     val br = row * 17 + col

                     // Now check if the upper-right and bottom-right corner have the same colour as others.
                     if (traitsequence(ul) != traitsequence(ur)
                        || traitsequence(ul) != traitsequence(br)) {
                        colViolations
                     } else {
                        colViolations + 1
                     }
                  }))
            }
         })
   }
}
