package com.derek.algs

import com.derek.algs.structures.concrete.FNTraitSeqVal
import com.derek.algs.structures.specification.TraitSeq

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
  def main(args: Array[String]) {
    println("\nGriewank")
    (-300 until 300)
      .foreach(i =>
      println(i / 3.0 + ": " + griewank(new FNTraitSeqVal(Array(i / 3.0), null))))

    println("\nRastrigin")
    (-300 until 300)
      .foreach(i =>
      println(i / 3.0 + ": " + rastrigin(new FNTraitSeqVal(Array(i / 3.0), null))))

    println("\nRosenbrock")
    (-300 until 300)
      .foreach(i =>
      println(i / 3.0 + ", " + 1 / 3.0 + ": " + rosenbrock(
        new FNTraitSeqVal(Array(i / 3.0, 1 / 3.0), null)))
      )
  }
}
