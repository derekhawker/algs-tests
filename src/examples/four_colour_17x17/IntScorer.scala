package examples.four_colour_17x17

import meta_heuristics.structures.specification.TraitSeq

import scala.annotation.tailrec

/**
 * Find the number of rectangles in 17x17 image. Rectangles here means the corner pieces with
 * the same colour.
 *
 * @author Derek Hawker
 */
trait IntScorer
{
   def traitScore(ts: TraitSeq[Int]): Double =
   {
      val colouringViolations = (0 until (Main.height - 1)).foldLeft {0}(
         (totalViolations, row) =>

            totalViolations
               + (0 until (Main.width - 1)).foldLeft(0)(
               (innerViolations, col) =>

                  innerViolations + numSameColourRectangles(ts, row, col)
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
      val ul = starty * Main.width + startx

      ((starty + 1) until Main.height).foldLeft(0)(
         (rowViolations,
          row) => {
            val bl = row * Main.width + startx

            // Only continue if upper-left and bottom-left corners are same colour
            if (traitsequence(bl) != traitsequence(ul)) {
               rowViolations + 0
            } else {
               (rowViolations
                  + ((startx + 1) until Main.width).foldLeft(0)(
                  (colViolations,
                   col) => {
                     val ur = starty * Main.width + col
                     val br = row * Main.width + col

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


trait IntScorerOptimized
{
   def traitScore(ts: TraitSeq[Int]): Double =
   {
      var colouringViolations = 0
      var row = 0

      while (row < Main.height) {
         var col = 0

         while (col < Main.width - 1) {
            colouringViolations += numSameColourRectangles(ts, row, col)
            col += 1
         }
         row += 1
      }
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
      var rowViolations = 0
      val ul = starty * Main.width + startx
      var row = starty + 1

      while (row < Main.height) {

         val bl = row * Main.width + startx

         // Only continue if upper-left and bottom-left corners are same colour
         if (traitsequence(bl) != traitsequence(ul)) {

            rowViolations + 0
         } else {

            var col = startx + 1

            while (col < Main.width) {

               val ur = starty * Main.width + col
               val br = row * Main.width + col

               // Now check if the upper-right and bottom-right corner have the same colour as others.
               if (traitsequence(ul) != traitsequence(ur)
                  || traitsequence(ul) != traitsequence(br)) {
               } else {
                  rowViolations += 1
               }

               col += 1
            }
         }
         row += 1
      }

      rowViolations
   }
}
