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
                    scorer: TraitSequence[T] => Double,
                    rng: Random) {
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
    val finalSolution = (0 until iterationLimit).foldLeft(
      (startingTraitSequeuence, startingTraitSequeuence))(
        (lastGen, i) => {
          val globalBest = lastGen._1
          val globalBestScore = scorer(globalBest)
          val localBest = lastGen._2


          println("^iteration: " + i)
          println("\t[Global]Best: (%f) %s".format(globalBestScore, globalBest))
          println("\t[Local]Best: (%f) %s".format(scorer(localBest), localBest))


          val bestNeighbourhoodMoves = Array.range(0, localBest.length)
            .map(move =>
            localBest.bestNeighbourhoodMove(move, scorer))

          val localMove = bestNeighbourhoodMoves.zipWithIndex
            .foldLeft(((localBest, Double.NegativeInfinity), -1))(
              (bestSol, bestNeighbourhoodMove) => {
                val neigbourScore = bestNeighbourhoodMove._1._2
                val bestSolutionScore = bestSol._1._2
                val move = bestNeighbourhoodMove._2

                // first see if it beats the current best of all the neighbours
                if (neigbourScore > bestSolutionScore) {
                  // check if on tabu list. if not, it becomes th best
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

          // pass to next iteration, the global best and the local best
          if (localMove._2 > globalBestScore)
            (localMove._1._1, localMove._1._1)
          else
            (globalBest, localMove._1._1)
        })

    val globalbest = finalSolution._1
    val localbest = finalSolution._2

    globalbest
  }
}
