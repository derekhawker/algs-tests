package meta_heuristics.particle_swarm_optimization.particle

/**
 * Update position, implementing wraparound when position exceeds allowable boundariess
 * @author Derek Hawker
 */
trait DoublePositionUpdate
{

   final def updatePosition(position: Double,
                      velocity: Double,
                      positionBounds: (Double, Double)): Double =
   {

      val lowerBounds = positionBounds._1
      val upperBounds = positionBounds._2
      val newPosition = ((position + velocity) % upperBounds) + lowerBounds

      if (newPosition < 0)
         newPosition + upperBounds
      else
         newPosition
   }

}
