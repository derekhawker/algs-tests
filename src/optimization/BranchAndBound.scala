package optimization

import meta_heuristics.util.ExecutableAlgorithm
import meta_heuristics.structures.specification.TraitSeq
import scala.collection.mutable
import optimization.BranchAndBound.{Solution, BranchAndBoundNode}
import scala.annotation.tailrec


/**
 * @author Derek Hawker
 */
abstract class BranchAndBound[T](patternSolution: TraitSeq[T],
                                 treeIterationOrdering: Ordering[Solution[T]],
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
   private val openSolutions = new mutable.PriorityQueue[Solution[T]]()(treeIterationOrdering)

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
                                incumbent: Option[TraitSeq[T]],
                                incumbentScore: Double,
                                openedSolutions: Array[BranchAndBoundNode[T]],
                                openedSolutionsScore: Array[Double]): Unit

   protected def isFeasible(seq: TraitSeq[T]): Boolean

   def getIndex(i: Int): Int

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

      // Can't branch on the last variable.
      // Don't explore branch if worse than incumbent score.
      // Assumption: It will always be worse (bounding function optimistic)
      if (branchLevel < dims && traitScore(node.solution) >= incumbentScore ) {

            val iterationStats =
               Array.range(0, node.nodes.length)
                  .map(i => {

                  // TODO: Needs to be extracted into a bounding function trait.
                  // Calculate the bounding functions for these new branches.
                  // Get root solution and modify
                  val updatedSol = node.solution.deepcopy()
                  updatedSol(getIndex(branchLevel)) = variableBounds(branchLevel)(i)

                  val updatedSolScore = traitScore(updatedSol)

                  // Create new Branching nodes
                  node.nodes(i) = Some(
                     new BranchAndBoundNode[T](numBranches(branchLevel), updatedSol, branchLevel))

                  // Add to list of open solutions
                  openSolutions.enqueue((node.nodes(i).get, updatedSolScore))

                  // update incumbent if reached a feasible solution
                  if (isFeasible(updatedSol) && updatedSolScore > newIncumbentScore) {
                     newIncumbent = Some(updatedSol)
                     newIncumbentScore = updatedSolScore
                  }

                  // Output some stats for printIteration
                  (node.nodes(i).get, updatedSolScore)
               })


            // Helpful output about this node
            val unzipped = iterationStats.unzip
            printIteration(iteration, level, newIncumbent, newIncumbentScore,
               unzipped._1.toArray, unzipped._2.toArray)
      }

      // Get next solution. Depends on the exploration criteria (depth-first, breadth-first,
      // or other method). May get nothing in which case we exit the method.
      if (openSolutions.size != 0) {

         val nextConsideredBranch = openSolutions.dequeue()._1
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

   override def execute(): TraitSeq[T] =
   {
      val root = new BranchAndBoundNode[T](numBranches(0), patternSolution, -1)

      val incumbentScore = incumbent match {
         case None => Double.NegativeInfinity
         case Some(i) => traitScore(i)
      }

      val best = innerExecute(root, incumbent, incumbentScore, 1)
      best
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