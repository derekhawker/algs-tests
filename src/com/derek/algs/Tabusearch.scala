package com.derek.algs

import com.derek.algs.structures.specification.TraitSeq
import com.derek.algs.util.ExecutableAlgorithm
import collection.JavaConversions._
import java.util.concurrent.ConcurrentHashMap
import scala.collection.JavaConverters._
import scala.collection.concurrent.Map

/**
 * Tabusearch. At each iteration, every neighbouring solution is evaluated. The move with the
 * highest increase in score is selected as the new solution. The move is put on the tabulist for
 * a specified period of time. We cannot use that move again until the tabu expires, UNLESS it
 * improves the global best score.
 *
 * @param startingTraitSequeuence Starting solution.
 * @param tabuTimeToLive Time to leave a move on tabu list.
 * @param iterationLimit Max number of algorithm iterations.
 * @param endOfIterationCondition Function to end algorithm before the maximum number of iterations
 *                                is reached.
 * @param iterationOutputPrinter Function to display output after each iteration.
 * @param scorer Function that evaluates each solution.
 * @tparam T conforms the the type of TraitSeq
 *
 * @author Derek Hawker
 */
class Tabusearch[T](val startingTraitSequeuence: TraitSeq[T],
                    val tabuTimeToLive: Int,
                    val iterationLimit: Int,
                    endOfIterationCondition: (Int, TraitSeq[T], Double, TraitSeq[T], Double) => Boolean,
                    iterationOutputPrinter: (Int, TraitSeq[T], Double, TraitSeq[T], Double) => Unit,
                    scorer: TraitSeq[T] => Double) extends ExecutableAlgorithm[T] {

  // If ttl is greater than the number of trait slots all moves become banned eventually.
  assert(tabuTimeToLive < startingTraitSequeuence.length)

  /**
   * tracks used moves and how many iterations till we can use them again.
   */
  val tabuList: Map[Int, Int] = new ConcurrentHashMap[Int, Int]().asScala

  /**
   * Decrement time-to-live on all keys(representing moves) and remove those that are 0
   */
  private def updateTabuList(): Unit = {
    tabuList.foreach(
      pair => {
        val k = pair._1
        val v = pair._2
        if (v == 0)
          tabuList.remove(k)
        else
          tabuList(k) = v - 1
      })
  }

  def execute(): TraitSeq[T] = {
    val finalSolution = innerRun()

    val globalbest = finalSolution._1
    val localbest = finalSolution._2

    globalbest
  }

  private def innerRun(): (TraitSeq[T], TraitSeq[T]) = {
    (0 until iterationLimit)
      .foldLeft((startingTraitSequeuence, startingTraitSequeuence))(
        (lastGen, i) => {

          val globalBest = lastGen._1
          val globalBestScore = scorer(globalBest)
          val lastLocal = lastGen._2


          // Find the optimum move for each individual trait
          val bestNeighbourhoodMoves = Array.range(0, lastLocal.length)
            .par.map(move =>
            lastLocal.bestNeighbourhoodMove(move, scorer)).seq.toArray

          // Find the highestScoringParticle move of all the moves calculated above
          val localMove = bestNeighbourhoodMoves.zipWithIndex
            .par.foldLeft(((lastLocal, Double.NegativeInfinity), -1))(
              (bestSol, bestNeighbourhoodMove) => {
                val neigbourScore = bestNeighbourhoodMove._1._2
                val bestSolutionScore = bestSol._1._2
                val move = bestNeighbourhoodMove._2

                // first see if a move beats the current highestScoringParticle of all checked so far
                if (neigbourScore > bestSolutionScore) {
                  /* Can only use a solution if the move that found it is not on tabu move list
                  or beats the global max */
                  if (!tabuList.contains(move) || neigbourScore > globalBestScore) {
                    (bestNeighbourhoodMove._1, move)
                  } else {
                    bestSol
                  }
                } else {
                  bestSol
                }
              })

          // Make sure that the default values were not used.
          assert(localMove._2 != -1)

          // Add selected move to tabu list.
          tabuList(localMove._2) = tabuTimeToLive
          updateTabuList()


          val localBest = localMove._1._1
          val localBestScore = localMove._1._2

          iterationOutputPrinter(i, globalBest, globalBestScore, localBest, localBestScore)

          /** **************************************************************************************
           Early exit if meeting certain conditions */
          val canContinue = endOfIterationCondition(i, globalBest, globalBestScore, localBest,
            localBestScore)
          if (!canContinue)
            return if (localMove._2 > globalBestScore)
                     (localBest, localBest)
                   else
                     (globalBest, localBest)

          /** *************************************************************************************/


          /* pass to next iteration: global highestScoringParticle, local highestScoringParticle.*/
          // Determine if new global highestScoringParticle.
          if (localMove._2 > globalBestScore)
            (localBest, localBest)
          else
            (globalBest, localBest)
        })
  }
}
