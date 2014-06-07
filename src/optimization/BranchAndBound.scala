package optimization

import meta_heuristics.util.ExecutableAlgorithm
import meta_heuristics.structures.specification.TraitSeq
import optimization.BranchAndBound.{Solution, BranchAndBoundNode}
import scala.annotation.tailrec
import java.util.{PriorityQueue, Comparator}


/**
 *
 * @author Derek Hawker
 *
 * @param patternSolution
 * @param treeIterationOrdering
 * @param incumbent A starting point to kick-start the b&b algorithm.
 * @param variableBounds
 * @tparam T
 */
abstract class BranchAndBound[@specialized(Char, Double, Int) T](patternSolution: TraitSeq[T],
                                                           treeIterationOrdering: Comparator[Solution[T]],
                                                           private var incumbent: Option[TraitSeq[T]],
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
                                openedSolutions: Array[BranchAndBoundNode[T]],
                                openedSolutionsScore: Array[Double]): Unit

   protected def isFeasible(seq: TraitSeq[T]): Boolean

   override def execute(): TraitSeq[T] =
   {
      val root = new BranchAndBoundNode[T](numBranches(0), patternSolution, -1)

      // Check if incumbent was provided.
      val incumbentScore = incumbent match {
         case None => Double.NegativeInfinity
         case Some(i) => -0.1
      }

      val best = innerExecute(root, incumbent, incumbentScore, 1)
      best
   }

   /**
    * Remove a node and its descendents from the open solutions queue.
    * @param stack remaining nodes to remove from queue.
    */
   @tailrec
   private def removeFromOpenSolution(stack: List[BranchAndBoundNode[T]]): Unit =
   {
      var st = stack

      stack match {
         case Nil =>
         case head :: tail =>
            openSolutions.remove(head)

            (0 until head.nodes.length)
               .foreach(n => head.nodes(n) match {
               case Some(nd) =>
                  st = nd :: st
               case None =>
            })

            removeFromOpenSolution(tail)
      }
   }

   @tailrec
   private def innerExecute(node: BranchAndBoundNode[T],
                            incumbent: Option[TraitSeq[T]],
                            incumbentScore: Double,
                            iteration: Int): TraitSeq[T] =
   {
      val level = node.level
      val branchLevel = level + 1

      var newIncumbent = incumbent
      var newIncumbentScore = incumbentScore

      // 1. Can't branch on the last variable.
      // 2. Don't explore branch if worse than incumbent score, because ...
      //    Assumption: It will always be worse (bounding function optimistic)
      if (branchLevel < dims && traitScore(node.solution) > incumbentScore) {

         val iterationStats =
            Array.range(0, node.nodes.length)
               .flatMap(i => {

               // TODO: Needs to be extracted into a bounding function trait.
               // Calculate the bounding functions for these new branches.
               // Get root solution and modify
               val updatedSol = node.solution.deepcopy()
               updatedSol(branchLevel) = variableBounds(branchLevel)(i)

               val updatedSolScore = traitScore(updatedSol)

               if (updatedSolScore > incumbentScore) {

                  // Create new Branching nodes
                  node.nodes(i) = Some(
                     new BranchAndBoundNode[T](numBranches(branchLevel), updatedSol, branchLevel))

                  // Add to list of open solutions
                  openSolutions.add((node.nodes(i).get, updatedSolScore))

                  // update incumbent if reached a feasible solution
                  if (isFeasible(updatedSol) && updatedSolScore > newIncumbentScore) {
                     newIncumbent = Some(updatedSol)
                     newIncumbentScore = updatedSolScore
                  }

                  // Output some stats for printIteration
                  Some((node.nodes(i).get, updatedSolScore))
               } else {

                  None
               }
            })


         // Helpful output about this node
         val unzipped = iterationStats.unzip
         printIteration(iteration, level, openSolutions, newIncumbent, newIncumbentScore,
            unzipped._1.toArray, unzipped._2.toArray)
      } else {

         // Incumbent was better than this solution. Remove it and dependents (fathoming)
         removeFromOpenSolution(List(node))
      }

      // Get next solution. Depends on the exploration criteria (depth-first, breadth-first,
      // or other method). May get nothing in which case we are finished and exit.
      if (openSolutions.size != 0) {

         val nextConsideredBranch = openSolutions.remove()._1
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
   type Solution[T] = (BranchAndBoundNode[T], Double)

   class BranchAndBoundNode[T](numLeafs: Int,
                               val solution: TraitSeq[T],
                               val level: Int)
   {
      // By default the nodes are unexplored
      val nodes: Array[Option[BranchAndBoundNode[T]]] =
         Array.range(0, numLeafs)
            .map(n => None)
   }
}