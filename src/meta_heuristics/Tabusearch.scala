package meta_heuristics

import meta_heuristics.structures.specification.TraitSeq
import meta_heuristics.util.ExecutableAlgorithm
import java.util.concurrent.ConcurrentHashMap
import scala.collection.JavaConverters._
import scala.collection.concurrent.Map
import scala.collection.parallel.mutable.ParArray
import meta_heuristics.output.DefaultIterationOutput

/**
 * Tabusearch. At each iteration, every neighbouring solution is evaluated. The move with the
 * highest increase in score is selected as the new solution. The move is put on the tabulist for
 * a specified period of time. We cannot use that move again until the tabu expires, UNLESS it
 * improves the global best score.
 *
 * @param currentSolution Starting solution.
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
abstract class Tabusearch[T](var currentSolution: TraitSeq[T],
                             val tabuTimeToLive: Int,
                             val iterationLimit: Int) extends ExecutableAlgorithm[T] with Serializable
{

   // If ttl is greater than the number of trait slots, __all__ moves become banned eventually.
   assert(tabuTimeToLive < currentSolution.length)

   val filename = "tabusearch.ser"

   var currentIteration = 0

   /**
    * tracks used moves and how many iterations till we can use them again.
    */
   val tabuList: Map[Int, Int] = new ConcurrentHashMap[Int, Int]().asScala

   def checkForConvergence(iteration: Int,
                           population: Array[TraitSeq[T]],
                           score: Array[Double]): Boolean

   def printIteration(iteration: Int,
                      population: Array[TraitSeq[T]],
                      scores: Array[Double],
                      globalBest: TraitSeq[T],
                      globalBestScore: Double,
                      localBest: TraitSeq[T],
                      localBestScore: Double)

   def traitScore(ts: TraitSeq[T]): Double


   /**
    * Decrement time-to-live on all keys(representing moves) and remove those that are 0
    */
   private def updateTabuList(): Unit =
   {
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

   def execute(): Option[TraitSeq[T]] =
   {
      val res = innerRun()

      val globalbest = res._1
      val localbest = res._2
      currentSolution = globalbest

      Some(globalbest)
   }

   private def innerRun(): (TraitSeq[T], TraitSeq[T]) =
   {
      (currentIteration until (currentIteration + iterationLimit))
         .foldLeft((currentSolution, currentSolution))(
            (lastGen, i) => {

               val globalBest = lastGen._1
               val globalBestScore = traitScore(globalBest)
               val lastLocal = lastGen._2

               currentIteration += 1


               // Find the optimum move for each individual trait
               val bestNeighbourhoodMoves = ParArray.range(0, lastLocal.length)
                  .map(move =>
                  lastLocal.bestNeighbourhoodMove(move, traitScore))

               // Find the highestScoringParticle move of all the moves calculated above
               // Even check the moves on tabu list since we can use them if they beat the global best
               val neighbourhoodSearchRes = bestNeighbourhoodMoves.zipWithIndex
                  .foldLeft(((lastLocal, Double.NegativeInfinity), -1))(
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


               val localBest = neighbourhoodSearchRes._1._1
               val localBestScore = neighbourhoodSearchRes._1._2
               val localMove = neighbourhoodSearchRes._2

               // Make sure that the default values were not used.
               assert(localMove != -1)

               // Add selected move to tabu list.
               tabuList(localMove) = tabuTimeToLive
               updateTabuList()


               printIteration(i, Array(globalBest, localBest),
                  Array(globalBestScore, localBestScore),
                  globalBest, globalBestScore, localBest, localBestScore)

               /** **************************************************************************************
           Early exit if meeting certain conditions */
               val canContinue = checkForConvergence(i, Array(globalBest, localBest),
                  Array(globalBestScore, localBestScore))

               if (!canContinue)
                  return if (localBestScore > globalBestScore)
                     (localBest, localBest)
                  else
                     (globalBest, localBest)

               /** *************************************************************************************/


               /* pass to next iteration: global highestScoringParticle, local highestScoringParticle.*/
               // Determine if new global highestScoringParticle.
               if (localBestScore > globalBestScore)
                  (localBest, localBest)
               else
                  (globalBest, localBest)
            })
   }
}


object Tabusearch
{
   /**
    * Initialize tabusearch with basic defaults.
    * 400 iterations
    * Time to live is set to 1/2 the length of traitseq
    *
    * @param startingSolution
    * @param score
    * @tparam T
    * @return
    */
   def defaultArguments[T](startingSolution: TraitSeq[T],
                           score: (TraitSeq[T]) => Double): Tabusearch[T] =
   {

      val numIterations = 400
      val tabuTimeToLive = startingSolution.length / 2

      new Tabusearch[T](startingSolution, tabuTimeToLive, numIterations)
         with IgnoredIterationConditionCheck[T] with DefaultIterationOutput[T]
      {
         override def traitScore(ts: TraitSeq[T]): Double =
            score.apply(ts)
      }

   }

   private def endOfIterationCondition[T](iteration: Int,
                                          globalBest: TraitSeq[T],
                                          globalBestScore: Double,
                                          localBest: TraitSeq[T],
                                          localBestScore: Double): Boolean =
   {
      if ((math.abs(localBestScore - 0.0) < 0.0000001)
         || (math.abs(globalBestScore - 0.0) < 0.0000001))
         false
      else
         true
   }
}