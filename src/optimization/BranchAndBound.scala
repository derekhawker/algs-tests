package optimization

import meta_heuristics.util.ExecutableAlgorithm
import meta_heuristics.structures.specification.TraitSeq
import scala.annotation.tailrec
import java.util.{PriorityQueue, Comparator}
import examples.four_colour_17x17.Main
import optimization.branch_and_bound.OpenSolution
import com.typesafe.scalalogging.slf4j.StrictLogging


/**
 *
 * @author Derek Hawker
 *
 * @param initialSolution
 * @param treeIterationOrdering
 * @param incumbent A starting point to kick-start the b&b algorithm.
 * @param variableBounds
 * @tparam T
 */
abstract class BranchAndBound[@specialized(Char, Double, Int) T](initialSolution: TraitSeq[T],
                                                                 treeIterationOrdering: Comparator[OpenSolution[T]],
                                                                 protected var incumbent: Option[TraitSeq[T]],
                                                                 val variableBounds: Array[Array[T]])
   extends ExecutableAlgorithm[T] with Serializable with StrictLogging
{
   /**
    * Determine the next bud branch to open. Override using a trait with desired method of exploring
    * tree. For example depth-first or breadth-first.
    *
    * Most promising solutions. Stored in decreasing order by their scores.
    * TODO: Need to handle concurrency
    */
   private val openSolutions = new PriorityQueue[OpenSolution[T]](10000, treeIterationOrdering)

   /**
    * The number of dimensions/variables for this problem.
    * @return the # of dimensions.
    */
   def dims: Int = variableBounds.length

   /**
    * Determine the number of branches that are possible for a given variable in solution
    * @param pos the index of the variable being considered for branching.
    * @return the # of branches.
    */
   def numBranches(pos: Int): Int = variableBounds(pos).length

   protected def traitScore(ts: TraitSeq[T]): Double

   protected def boundedTraitScore(ts: TraitSeq[T],
                                   level: Int): Double

   protected def printIteration(iteration: Int,
                                level: Int,
                                openSolutions: PriorityQueue[OpenSolution[T]],
                                incumbent: Option[TraitSeq[T]],
                                incumbentScore: Double,
                                openedSolutions: Array[TraitSeq[T]],
                                openedSolutionsScore: Array[Double]): Unit

   protected def isFeasible(seq: TraitSeq[T]): Boolean

   protected def boundingTrait(ts: TraitSeq[T],
                               branchLevel: Int,
                               decisionVariableValue: Int): TraitSeq[T]

   /**
    * Begin B&B.
    * @return May not be able to find feasible solution (out of memory, not feasible), in which case
    *         returns None.
    */
   override def execute(): Option[TraitSeq[T]] =
   {
      // Check if incumbent was provided.
      val (root, incumbentCheck) = incumbent match {
         case None =>
            (initialSolution,
               (None, Double.NegativeInfinity))
         case Some(i) =>
            (initialSolution,
               (incumbent, boundedTraitScore(i, 0)))
      }


      // Run until we get out of memory.
      try {

         Some(innerExecute(
            OpenSolution(root, Double.PositiveInfinity, -1),
            incumbentCheck._1, incumbentCheck._2, 1))
      } catch {
         case e: OutOfMemoryError =>
            logger.warn("OUT OF MEMORY.")
            logger.warn(e.getMessage)
            logger.warn(e.getStackTrace.mkString("\n"))
            incumbent
      }
   }

   @tailrec
   private def innerExecute(openNode: OpenSolution[T],
                            incumbent: Option[TraitSeq[T]],
                            incumbentScore: Double,
                            iteration: Int): TraitSeq[T] =
   {
      val parentSolution = openNode.parentSolution
      val boundingValue = openNode.boundingScore

      val level = openNode.level
      val branchLevel = level + 1

      var newIncumbent = incumbent
      var newIncumbentScore = incumbentScore

      // 1. Can't branch on the last variable.
      // 2. Don't explore branch if worse/equal to the incumbent score, because ...
      //    Assumption: It can only stay the same or get worse (bounding function optimistic).
      if (branchLevel < dims && boundingValue > incumbentScore) {

         val iterationStats =
            (0 until variableBounds(branchLevel).length)
               .flatMap(i => {

               // Calculate the bounding functions for these new branches.
               // Get parentSolution solution and modify
               val branchSol = boundingTrait(parentSolution, branchLevel, i)
               val branchScore = boundedTraitScore(branchSol, branchLevel)

               if (branchScore > incumbentScore) {

                  // Add to list of open solutions
                  openSolutions.add(
                     OpenSolution(branchSol, branchScore, branchLevel))

                  // update incumbent if reached a feasible solution
                  if (isFeasible(branchSol) && branchScore > newIncumbentScore) {
                     newIncumbent = Some(branchSol)
                     // Recalculate
                     newIncumbentScore = traitScore(branchSol)
                  }

                  // Output some stats for printIteration
                  Some((branchSol, branchScore))
               } else {

                  None
               }
            })


         // Helpful output about this openNode
         val unzipped = iterationStats.unzip
         printIteration(iteration, level, openSolutions, newIncumbent, newIncumbentScore,
            unzipped._1.toArray, unzipped._2.toArray)
      } else {

         // When this node was originally added to the open solutions queue, the incumbent was
         // worse than the bounded value. Since that's no longer the case. Fetch another open sol'n.
      }

      // Get next solution. Depends on the exploration criteria (depth-first, breadth-first,
      // or other method). May get nothing in which case we are finished and exit.
      if (openSolutions.size != 0) {

         val nextConsideredBranch = openSolutions.remove()
         innerExecute(nextConsideredBranch, newIncumbent, newIncumbentScore, iteration + 1)
      } else {

         incumbent match {
            case None =>
               throw new RuntimeException("Unable to find a feasible solution")
            case Some(sol) =>
               sol
         }
      }
   }
}


object BranchAndBound
{
   val bestFirstOrdering = new Comparator[OpenSolution[Int]]
   {
      override def compare(x: OpenSolution[Int], y: OpenSolution[Int]): Int =
         (y.boundingScore - x.boundingScore).toInt
   }

   val depthFirstOrdering = new Comparator[OpenSolution[Int]]
   {
      override def compare(x: OpenSolution[Int], y: OpenSolution[Int]): Int =
         y.level - x.level
   }

   val breadthFirstOrdering = new Comparator[OpenSolution[Int]]
   {
      override def compare(x: OpenSolution[Int], y: OpenSolution[Int]): Int =
         x.level - y.level
   }

   val depthBestFirstOrdering = new Comparator[OpenSolution[Int]]
   {
      override def compare(x: OpenSolution[Int], y: OpenSolution[Int]): Int =
         ((y.boundingScore + (Main.numFeatures - y.level))
            - (x.boundingScore + (Main.numFeatures - x.level))).toInt
   }
}
