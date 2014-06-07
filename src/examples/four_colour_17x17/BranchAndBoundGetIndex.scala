package examples.four_colour_17x17

/**
 * @author Derek Hawker
 */
trait BranchAndBoundGetIndex
{
   private var buildIndex = getIndexes

   private def getIndexes: Array[Int] =
   {
      val indexesUsed = Array.range(0, 17 * 17).map(i => false)
      val indexMapper = Array.range(0, 17 * 17).map(i => -1)

      var indexUnused = 0
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
      assert(indexUnused == 17*17)

      indexMapper
   }

   def getIndex(i: Int): Int = buildIndex(i)
}
