package meta_heuristics

import meta_heuristics.structures.specification.TraitSeq
import meta_heuristics.util.ExecutableAlgorithm
import meta_heuristics.particle_swarm_optimization.particle.Particle
import meta_heuristics.output.DefaultPSOIterationOutput

/**
 *
 * @param population Starting population.
 * @param velocityFollow
 * @param globalOptimumFollow
 * @param localOptimumFollow
 * @param numIterations
 * @param updateVelocity
 * @param endOfIterationCondition
 * @param iterationOutputPrinter
 * @param scorer
 * @tparam T Conforms to the type of TraitSeq
 *
 * @author Derek Hawker
 */
abstract class ParticleSwarm[T](var population: Array[Particle[T]],
                                val positionBounds: Array[(T, T)],
                                val velocityFollow: Double,
                                val globalOptimumFollow: Double,
                                val localOptimumFollow: Double,
                                val numIterations: Int) extends ExecutableAlgorithm[T] with Serializable
{

   var currentIteration = 0
   val filename         = "pso.ser"

   def updateVelocity(position: T,
                      velocity: T,
                      localBestPosition: T,
                      globalBestPosition: T,
                      velocityFollow: Double,
                      globalOptimumFollow: Double,
                      localOptimumFollow: Double): T

   def updatePosition(position: T,
                      velocity: T,
                      positionBounds: (T, T)): T

   def checkForConvergence(iteration: Int,
                           population: Array[Particle[T]],
                           globalBest: Particle[T],
                           globalBestScore: Double,
                           localBest: Particle[T],
                           localBestScore: Double): Boolean

   def printIteration(iteration: Int,
                      population: Array[Particle[T]],
                      scores: Array[Double],
                      globalBest: Particle[T], globalBestScore: Double,
                      localBest: Particle[T],
                      localBestScore: Double): Unit

   def traitScore(ts: TraitSeq[T]): Double

   override def execute(): Option[TraitSeq[T]] =
   {
      // Need a starting global best
      val scores = population.map(p => traitScore(p.position))

      val startingGlobalBest = population.tail.zip(scores.tail)
         .foldLeft((population.head, scores.head))(
            scoreParticle)


      val results = innerExecute(startingGlobalBest)
      val pop = results._1
      population = pop

      val bestParticle = results._2._1
      Some(bestParticle.position)
   }


   private def innerExecute(startingGlobalBest: (Particle[T], Double)):
   (Array[Particle[T]], (Particle[T], Double)) =
   {

      (currentIteration until currentIteration + numIterations)
         .foldLeft((population, startingGlobalBest))(
            (state,
             i) => {
               val pop = state._1
               val globalBest = state._2._1
               val globalBestScore = state._2._2

               currentIteration += 1

               val newpopScores = pop
                  .par.map(p => {
                  val copy = p.deepcopy()

                  copy.zip(globalBest.zipWithIndex).foreach(
                     tr => {
                        val i = tr._2._2

                        val currentParticle = tr._1
                        val pos = currentParticle._1
                        val vel = currentParticle._2
                        val localBestPos = currentParticle._3

                        val globalBestParticle = tr._2._1
                        val globalBestPos = globalBestParticle._1


                        val newVelocity = updateVelocity(pos, vel, localBestPos, globalBestPos,
                           velocityFollow, globalOptimumFollow, localOptimumFollow)

                        val newPosition = updatePosition(pos, newVelocity, positionBounds(i))

                        copy.position(i) = newPosition
                        copy.velocity(i) = newVelocity
                     })

                  val score = traitScore(copy.position)
                  val oldScore = traitScore(copy.localBest)

                  if (score > oldScore)
                     (new Particle[T](copy.position, copy.velocity, copy.position, null), score)
                  else
                     (copy, score)
               }).seq.toArray

               val bestParticle = newpopScores.tail
                  .foldLeft((newpopScores.head))(
                     scoreParticle)

               val newpop: Array[Particle[T]] = newpopScores.map(_._1)
               val scores = newpopScores.map(_._2)

               printIteration(i, newpop, scores,
                  globalBest, globalBestScore,
                  bestParticle._1, bestParticle._2)


               /** **************************************************************************************
           Early exit if meeting certain conditions */
               val canContinue = checkForConvergence(i, newpop, globalBest,
                  globalBestScore, bestParticle._1, bestParticle._2)
               if (!canContinue)
                  return if (bestParticle._2 > globalBestScore)
                     (newpop, bestParticle)
                  else
                     (newpop, state._2)

               /** *************************************************************************************/

               if (bestParticle._2 > globalBestScore)
                  (newpop, bestParticle)
               else
                  (newpop, state._2)
            })
   }

   /**
    * Used in a fold to evaluate a population of particles to find the particle with the highest
    * score
    *
    * @param highestScoringParticle Tuple2(_1: particle, _2: its score).
    * @param currentParticle
    * @return highest scoring (particle, score) pair
    */
   def scoreParticle(highestScoringParticle: (Particle[T], Double),
                     currentParticle: (Particle[T], Double)): (Particle[T], Double) =
   {
      val bestScore = highestScoringParticle._2
      val currScore = currentParticle._2

      if (currScore > bestScore)
         currentParticle
      else
         highestScoringParticle
   }
}


object ParticleSwarm
{
   val velocityFollow      = 1
   val localOptimumFollow  = 0.3
   val globalOptimumFollow = 0.7
   val numIterations       = 200


   def defaultArguments[T](population: Array[Particle[T]],
                           positionBounds: Array[(T, T)],
                           updateVelocity: (T, T, T, T, Double, Double, Double) => T,
                           updatePosition: (T, T, (T, T)) => T,
                           score: TraitSeq[T] => Double): ParticleSwarm[T] =
   {

      new ParticleSwarm[T](population, positionBounds,
         velocityFollow, globalOptimumFollow, localOptimumFollow, numIterations)
         with IgnoredPSOCondition[T] with DefaultPSOIterationOutput[T]
      {
         override def updateVelocity(position: T, velocity: T, localBestPosition: T,
                                     globalBestPosition: T, velocityFollow: Double,
                                     globalOptimumFollow: Double,
                                     localOptimumFollow: Double): T = throw new RuntimeException(
            "not implemented")

         override def updatePosition(position: T, velocity: T,
                                     positionBounds: (T, T)): T = throw new RuntimeException(
            "not implemented")

         override def traitScore(ts: TraitSeq[T]): Double =
            score.apply(ts)

      }
   }
}