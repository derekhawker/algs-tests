package meta_heuristics.genetic_algorithms.babies

import meta_heuristics.structures.specification.TraitSeq
import scala.util.Random

/**
 * @author Derek Hawker
 */
trait SpliceParents[T]
{
  /**
   * Create children based on splicing
   *
   * @param parents
   * @return
   */
  def makeBabies(parents: Array[TraitSeq[T]]): Array[TraitSeq[T]] = {
    assert(parents.length == 2)

    val (p1, p2) = (parents(0), parents(1))
    assert(p1.length == p2.length) // Condition that they both have same length

    val splitPoint = Random.nextInt(p1.length) + 1

    val child1 = p1.deepcopy()
    val child2 = p2.deepcopy()

    /* TODO: Probably broken for problems where we have reference types and the neighbourhood isn't
    finite. Like searching for weights. If you assign directly to a reference and then modify the
    reference, then you're going to modify a trait in two different traitsequences
     */
    (0 until splitPoint)
      .foreach(i =>
      child2(i) = child1(i))

    (splitPoint until p1.length)
      .foreach(i =>
      child1(i) = child2(i))

    Array(child1, child2)
  }
}
