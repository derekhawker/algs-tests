package com.derek.algs

import com.derek.algs.structures.{TraitSequence}
import scala.util.Random
import scala.collection.mutable

/**
 * @author Derek Hawker
 */
class Tabusearch[T](val startingTraitSequeuence: TraitSequence[T],
                    val tabuTimeToLive: Int,
                    val iterationLimit: Int,
                    rng: Random,
                    endOfIterationCondition: (Int, TraitSequence[T], Double, TraitSequence[T], Double) => Boolean,
                    scorer: TraitSequence[T] => Double) {

  val tabuList = mutable.HashMap[Int, Int]()

  /**
   * Decrement time-to-live on all keys(representing moves) and remove those that are 0
   */
  def updateTabuList(): Unit = {
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

  def run(): TraitSequence[T] = {
    val finalSolution = innerRun()
    val globalbest = finalSolution._1
    val localbest = finalSolution._2

    globalbest
  }

  private def innerRun(): (TraitSequence[T], TraitSequence[T]) = {
    (0 until iterationLimit).foldLeft(
      (startingTraitSequeuence, startingTraitSequeuence))(
        (lastGen, i) => {

          val globalBest = lastGen._1
          val globalBestScore = scorer(globalBest)
          val lastLocal = lastGen._2


          val bestNeighbourhoodMoves = Array.range(0, lastLocal.length)
            .map(move =>
            lastLocal.bestNeighbourhoodMove(move, scorer))

          val localMove = bestNeighbourhoodMoves.zipWithIndex
            .foldLeft(((lastLocal, Double.NegativeInfinity), -1))(
              (bestSol, bestNeighbourhoodMove) => {
                val neigbourScore = bestNeighbourhoodMove._1._2
                val bestSolutionScore = bestSol._1._2
                val move = bestNeighbourhoodMove._2

                // first see if it beats the current best of all the neighbours
                if (neigbourScore > bestSolutionScore) {
                  //Can only use a score if not on tabu move list or beats the global max
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

          updateTabuList()
          tabuList(localMove._2) = tabuTimeToLive


          val localBest = localMove._1._1
          val localBestScore = localMove._1._2

          println("^iteration: " + i)
          println("\t[Global]Best: (%f) %s".format(globalBestScore, globalBest))
          println("\t[Local]Best: (%f) %s".format(localBestScore, localBest))

          /** **************************************************************************************
           Early exit if meeting certain conditions
            */
          val canContinue = endOfIterationCondition(i, globalBest, globalBestScore, localBest,
            localBestScore)
          if (!canContinue)
            return if (localMove._2 > globalBestScore)
                     (localBest, localBest)
                   else
                     (globalBest, localBest)

          /** **************************************************************************************
            */


          // pass to next iteration, the global best and the local best
          if (localMove._2 > globalBestScore)
            (localBest, localBest)
          else
            (globalBest, localBest)
        })
  }
}
