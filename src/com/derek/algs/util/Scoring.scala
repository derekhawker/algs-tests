package com.derek.algs.util

import com.derek.algs.structures.specification.TraitSeq
import com.derek.algs.structures.concrete.finite.neighbourhood.TraitSeqVal

/**
 * @author Derek Hawker
 */
object Scoring {

  /**
   * @author Derek Hawker
   *
   * @param traitsequence
   * @return
   */
  def rosenbrock(traitsequence: TraitSeq[Double]): Double =
    (0 until traitsequence.length - 1)
      .foldLeft(0.0)(
        (count, i) => {
          (count
            + 100 * math.pow(traitsequence(i + 1)
            - math.pow(traitsequence(i), 2)
            , 2)
            + math.pow(traitsequence(i) - 1, 2))
        })


  /**
   * @author Derek Hawker
   *
   * @param traitsequence
   * @return
   */
  def rastrigin(traitsequence: TraitSeq[Double]): Double =
    -(10 * traitsequence.length
      + traitsequence.foldLeft(0.0)(
      (count, d) => {
        count + math.pow(d, 2) - 10 * math.cos(2 * math.Pi * d)
      }))


  /**
   *
   * @param traitsequence
   * @return
   */
  def griewank(traitsequence: TraitSeq[Double]): Double =
    -(1 +
      (traitsequence.foldLeft(0.0)(
        (count, d) =>
          count + math.pow(d, 2))
        / 4000)
      - traitsequence.zipWithIndex
      .foldLeft(1.0)(
        (count, pair) => {
          val i = pair._2
          count * math.cos(pair._1 / math.sqrt(i + 1))
        }
      ))

  def griewankInt(traitsequence: TraitSeq[Int]): Double =
    -(1 +
      (traitsequence.foldLeft(0.0)(
        (count, d) =>
          count + math.pow(d, 2))
        / 4000)
      - traitsequence.zipWithIndex
      .foldLeft(1.0)(
        (count, pair) => {
          val i = pair._2
          count * math.cos(pair._1 / math.sqrt(i + 1))
        }
      ))


  /**
   * Find the number of rectangles in 17x17 image. Rectangles here means the corner pieces with
   * the same colour.
   *
   * @author Derek Hawker
   * @param traitsequence
   * @return
   */
  def fourColour17x17Scorer(traitsequence: TraitSeq[Int]): Double = {
    val ts = traitsequence.asInstanceOf[TraitSeqVal[Int]]

    val colouringViolations = (0 until (17 - 1)).foldLeft {0}(
      (totalViolations, row) =>
        totalViolations
          + (0 until (17 - 1)).foldLeft(0)(
          (colViolations, col) =>
            colViolations
              + numSameColourRectangles(traitsequence, row, col)
        ))

    // The less violations, the higher the score
    -colouringViolations
  }

  /**
   * Helper function that searches all squares to the left and right from current square to find
   * squares that have the same colour and form the corner points of a rectangle.
   *
   * @author Derek Hawker
   * @param traitsequence
   * @param starty
   * @param startx
   * @return
   */
  def numSameColourRectangles(traitsequence: TraitSeq[Int], starty: Int, startx: Int): Int = {
    val ul = starty * 17 + startx

    ((starty + 1) until 17).foldLeft(0)(
      (rowViolations, row) => {
        val bl = row * 17 + startx

        // Only continue if upper-left and bottom-left corners are same colour
        if (traitsequence(bl) != traitsequence(ul)) {
          rowViolations + 0
        } else {
          (rowViolations
            + ((startx + 1) until 17).foldLeft(0)(
            (colViolations, col) => {
              val ur = starty * 17 + col
              val br = row * 17 + col

              // Now check if the upper-right and bottom-right corner have the same colour as others.
              if (traitsequence(ul) != traitsequence(ur)
                || traitsequence(ul) != traitsequence(br)) {
                colViolations
              } else {
                colViolations + 1
              }
            }))
        }
      })
  }

  def main(args: Array[String]) {
    println("\nGriewank")
    (-300 until 300)
      .foreach(i =>
      println(i / 3.0 + ": " + griewank(new TraitSeqVal(Array(i / 3.0), null))))

    println("\nRastrigin")
    (-300 until 300)
      .foreach(i =>
      println(i / 3.0 + ": " + rastrigin(new TraitSeqVal(Array(i / 3.0), null))))

    println("\nRosenbrock")
    (-300 until 300)
      .foreach(i =>
      println(i / 3.0 + ", " + 1 / 3.0 + ": " + rosenbrock(
        new TraitSeqVal(Array(i / 3.0, 1 / 3.0), null)))
      )
  }
}
