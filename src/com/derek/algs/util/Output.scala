package com.derek.algs.util

import scala.util.Random
import com.derek.algs.structures.specification.TraitSeq
import com.derek.algs.{Tabusearch, GeneticAlgorithm}
import com.derek.algs.particle.swarm.optimization.Particle
import com.derek.algs.examples.FourColour17x17Main
import com.derek.algs.util.gif.AnimatedProgressGif

/**
 * @author Derek Hawker
 */
object Output {
  def gaGenerationPrinter[T](generation: Int,
                           population: Array[TraitSeq[T]],
                           scores: Array[Double],
                           globalBest: (TraitSeq[T], Double),
                           genBest: (TraitSeq[T], Double)) {
    println("^generation: " + generation)

    val mean: Double = scores.sum / scores.length
    println("\tmean: " + mean
      + ", std.dev: " + math.sqrt(scores.foldLeft(0.0)(
      (count, s) =>
        count + math.pow(s - mean, 2)) / scores.length))

    println("\t[Global]Best: (%f) %s".format(globalBest._2, globalBest._1))
    println("\t[Local]Best: (%f) %s".format(genBest._2, genBest._1))
    AnimatedProgressGif.apply.addFrame(genBest._1.asInstanceOf[TraitSeq[Int]])
  }


  def tabusearchIterationPrinter[T](i: Int,
                          globalBest: TraitSeq[T],
                          globalBestScore: Double,
                          localBest: TraitSeq[T],
                          localBestScore: Double) {
    println("^iteration: " + i)
    println("\t[Global]Best: (%f) %s".format(globalBestScore, globalBest))
    println("\t[Local]Best: (%f) %s".format(localBestScore, localBest))
    AnimatedProgressGif.apply.addFrame(localBest.asInstanceOf[TraitSeq[Int]])
  }



  def psoIterationPrinter[T](i: Int,
                          population: Array[Particle[T]],
                          globalBest: Particle[T],
                          globalBestScore: Double) {
    println("^iteration: " + i)
    println("\t[Global]Best: (%f) %s".format(globalBestScore, globalBest.position))
    AnimatedProgressGif.apply.addFrame(globalBest.position.asInstanceOf[TraitSeq[Int]])
    //population
    //  .foreach(p => {
    //  println("\t" + p.position + " " + p.velocity + "score: " + Scoring.fourColour17x17Scorer(
    //    p.position.asInstanceOf[FNTraitSeqVal[Int]]))
    //})
  }

}
