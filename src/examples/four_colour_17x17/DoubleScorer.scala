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
      val colouringViolations = (0 until (Main.height - 1)).foldLeft {0}(
         (totalViolations,
          row) =>
            totalViolations
               + (0 until (Main.width - 1)).foldLeft(0)(
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
      val ul = starty * Main.width + startx

      ((starty + 1) until Main.height).foldLeft(0)(
         (rowViolations,
          row) => {
            val bl = row * Main.width + startx

            // Only continue if upper-left and bottom-left corners are same colour
            if (traitsequence(bl).toInt != traitsequence(ul).toInt) {
               rowViolations + 0
            } else {
               (rowViolations
                  + ((startx + 1) until Main.width).foldLeft(0)(
                  (colViolations,
                   col) => {
                     val ur = starty * Main.width + col
                     val br = row * Main.width + col

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
