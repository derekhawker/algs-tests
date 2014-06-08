package examples.four_colour_17x17

import meta_heuristics.structures.specification.TraitSeq

/**
 * Find the number of rectangles in 17x17 image. Rectangles here means the corner pieces with
 * the same colour.
 *
 * @author Derek Hawker
 */
trait IntScorer
{
   final def traitScore(ts: TraitSeq[Int]): Double =
   {
      val colouringViolations = (0 until (17 - 1)).foldLeft {0}(
         (totalViolations,
          row) =>
            totalViolations
               + (0 until (17 - 1)).foldLeft(0)(
               (colViolations,
                col) =>
                  colViolations
                     + numSameColourRectangles(ts, row, col)
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
         (rowViolations,
          row) => {
            val bl = row * 17 + startx

            // Only continue if upper-left and bottom-left corners are same colour
            if (traitsequence(bl) != traitsequence(ul)) {
               rowViolations + 0
            } else {
               (rowViolations
                  + ((startx + 1) until 17).foldLeft(0)(
                  (colViolations,
                   col) => {
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
