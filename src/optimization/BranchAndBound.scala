package optimization

import meta_heuristics.util.ExecutableAlgorithm
import meta_heuristics.structures.specification.TraitSeq
import optimization.BranchAndBound.{Solution, BranchAndBoundNode}
import scala.annotation.tailrec
import java.util.{PriorityQueue, Comparator}
import scala.collection.mutable.ListBuffer


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
                                                                 treeIterationOrdering: Comparator[Solution[T]],
                                                                 protected var incumbent: Option[TraitSeq[T]],
                                                                 val variableBounds: Array[Array[T]])
   extends ExecutableAlgorithm[T] with Serializable
{
   /**
    * Determine the next bud branch to open. Override using a trait with desired method of exploring
    * tree. For example depth-first or breadth-first.
    *
    * Most promising solutions. Stored in decreasing order by their scores.
    * TODO: Need to handle concurrency
    */
   private val openSolutions = new PriorityQueue[Solution[T]](10000, treeIterationOrdering)

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

   protected def printIteration(iteration: Int,
                                level: Int,
                                openSolutions: PriorityQueue[Solution[T]],
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
            (new BranchAndBoundNode[T](initialSolution, -1),
               (None, Double.NegativeInfinity))
         case Some(i) =>
            (new BranchAndBoundNode[T](initialSolution, -1),
               (incumbent, traitScore(i)))
      }


      // Run until we get out of memory.
      try {

         Some(innerExecute((root, 0, Double.PositiveInfinity),
            incumbentCheck._1, incumbentCheck._2, 1))
      } catch {
         case e: OutOfMemoryError =>
            incumbent
      }
   }

   @tailrec
   private def innerExecute(openNode: Solution[T],
                            incumbent: Option[TraitSeq[T]],
                            incumbentScore: Double,
                            iteration: Int): TraitSeq[T] =
   {
      val root = openNode._1
      val branchId = openNode._2
      val boundingValue = openNode._3

      val level = root.level + 1
      val branchLevel = level + 1

      var newIncumbent = incumbent
      var newIncumbentScore = incumbentScore

      // 1. Can't branchId on the last variable.
      // 2. Don't explore branchId if worse than incumbent score, because ...
      //    Assumption: It will always be worse (bounding function optimistic)
      if (branchLevel < dims && boundingValue > incumbentScore) {

         val branchedSol = boundingTrait(root.solution, level, branchId)
         val branchedNode = new BranchAndBoundNode[T](branchedSol, level)
         root.nodes += branchedNode
         println("Branch: " + boundingValue)

         val iterationStats =
            (0 until variableBounds(branchLevel).length)
               .flatMap(i => {

               // Calculate the bounding functions for these new branches.
               // Get root solution and modify
               val updatedSol = boundingTrait(branchedSol, branchLevel, i)

               val updatedSolScore = traitScore(updatedSol) // + dims - branchLevel

               if (updatedSolScore > incumbentScore) {

                  // Add to list of open solutions
                  openSolutions.add((branchedNode, i, updatedSolScore))

                  // update incumbent if reached a feasible solution
                  if (isFeasible(updatedSol) && updatedSolScore > newIncumbentScore) {
                     newIncumbent = Some(updatedSol)
                     newIncumbentScore = updatedSolScore
                  }

                  // Output some stats for printIteration
                  Some((updatedSol, updatedSolScore))
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
   type Solution[T] = (BranchAndBoundNode[T], Int, Double)

   class BranchAndBoundNode[T](val solution: TraitSeq[T],
                               val level: Int)
   {
      // By default the nodes are unexplored
      val nodes: ListBuffer[BranchAndBoundNode[T]] = ListBuffer()
   }
}