package com.derek.algs.examples

import scala.util.Random
import com.derek.algs.util.{Scoring, Output, TimedExecution}
import com.derek.algs.ParticleSwarmOptimization
import com.derek.algs.structures.concrete.FNTraitSeqVal
import com.derek.algs.structures.specification.TraitSeq
import com.derek.algs.particle.swarm.optimization.Particle

/**
 * @author Derek Hawker
 */
object FourColour17x17Main {
  def main(args: Array[String]) {
    //new TimedExecution().execute {
    //val numFeatures: Int = 17 * 17
    //val numGenerations = 400
    //val numPopulation = 2800
    //val mutationRate = 0.4
    //
    //val tabuTimeToLive = 5
    //
    //val neighbourhood = Array.range(0, numFeatures)
    //  .map(i =>
    //  Array.range(0, 4)
    //  )
    //
    //val population = Array.range(0, numPopulation)
    //  .map(person =>
    //  new FNTraitSeqVal(Array.range(0, numFeatures)
    //    .map(tr =>
    //    Random.nextInt(4)),
    //    neighbourhood).asInstanceOf[TraitSeq[Int]])
    //
    //  val best = new Tabusearch[Int](population.head, tabuTimeToLive, numGenerations,
    //    endOfIterationCondition, Output.iterationPrinter, fourColour17x17Scorer)
    //    .execute()
    //
    //  println(best)
    //  best
    //}
    //
    //
    //
    //new TimedExecution().execute {
    //val numFeatures: Int = 17 * 17
    //val numGenerations = 400
    //val numPopulation = 2800
    //val mutationRate = 0.4
    //
    //val tabuTimeToLive = 5
    //
    //val neighbourhood = Array.range(0, numFeatures)
    //  .map(i =>
    //  Array.range(0, 4)
    //  )
    //
    //val population = Array.range(0, numPopulation)
    //  .map(person =>
    //  new FNTraitSeqVal(Array.range(0, numFeatures)
    //    .map(tr =>
    //    Random.nextInt(4)),
    //    neighbourhood).asInstanceOf[TraitSeq[Int]])
    //
    //  val best = new GeneticAlgorithm[Int](population, numGenerations, mutationRate,
    //    endOfGenerationCondition, GeneticAlgorithm.Mating.eliteSelection,
    //    GeneticAlgorithm.BabyMaker.spliceParents, Output.generationPrinter, fourColour17x17Scorer)
    //    .execute()
    //
    //  println(best)
    //  best
    //}


    new TimedExecution().execute {
      val velocityFollow = 1
      val localOptimumFollow = 0.01
      val globalOptimumFollow = 0.01
      val numFeatures: Int = 17 * 17
      val numIterations = 400
      val numPopulation = 50

      val neighbourhood = Array.range(0, numFeatures)
        .map(i =>
        Array.range(0, 4)
        )

      val population = Array.range(0, numPopulation)
        .map(person => {

        val startPos: FNTraitSeqVal[Int] = new FNTraitSeqVal(
          Array.range(0, numFeatures)
            .map(tr =>
            Random.nextInt(4)),
          neighbourhood)

        new Particle[Int](startPos.asInstanceOf[TraitSeq[Int]],
          new FNTraitSeqVal(
            Array.range(0, numFeatures)
              .map(tr =>
              Random.nextInt(4)),
            neighbourhood).asInstanceOf[TraitSeq[Int]],
          startPos)

      })


      val best = new ParticleSwarmOptimization[Int](population, velocityFollow, globalOptimumFollow,
        localOptimumFollow, numIterations, Particle.updateVelocityInt,
        Output.psoIterationPrinter, Scoring.fourColour17x17Scorer)
        .execute()

      println(best)
      best
    }

  }

  private def endOfGenerationCondition[T](generation: Int,
                                          population: Array[TraitSeq[T]],
                                          scores: Array[Double]): Boolean = {

    true
  }

  private def endOfIterationCondition[T](iteration: Int,
                                         globalBest: TraitSeq[T],
                                         globalBestScore: Double,
                                         localBest: TraitSeq[T],
                                         localBestScore: Double): Boolean = {
    true
  }
}