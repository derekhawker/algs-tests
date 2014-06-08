package examples.four_colour_17x17

import meta_heuristics.structures.specification.TraitSeq
import meta_heuristics.structures.concrete.infinite.neighbourhood.INTraitSeqVal

/**
 * @author Derek Hawker
 */
trait DoubleScorer
{
   final def traitScore(ts: TraitSeq[Double]): Double =
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

   private def numSameColourRectangles(traitsequence: TraitSeq[Double],
                               starty: Int,
                               startx: Int): Int =
   {
      val ul = starty * 17 + startx

      ((starty + 1) until 17).foldLeft(0)(
         (rowViolations,
          row) => {
            val bl = row * 17 + startx

            // Only continue if upper-left and bottom-left corners are same colour
            if (traitsequence(bl).toInt != traitsequence(ul).toInt) {
               rowViolations + 0
            } else {
               (rowViolations
                  + ((startx + 1) until 17).foldLeft(0)(
                  (colViolations,
                   col) => {
                     val ur = starty * 17 + col
                     val br = row * 17 + col

                     // Now check if the upper-right and bottom-right corner have the same colour as others.
                     if (traitsequence(ul).toInt != traitsequence(ur).toInt
                        || traitsequence(ul).toInt != traitsequence(br).toInt) {
                        colViolations
                     } else {
                        colViolations + 1
                     }
                  }))
            }
         })
   }
}
