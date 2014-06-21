package meta_heuristics.util

import scala.collection.mutable.ArrayBuffer
import com.typesafe.scalalogging.Logging

import org.slf4j.LoggerFactory
import com.typesafe.scalalogging.slf4j.Logger


/**
 * @author Derek Hawker
 */
class TimedExecution extends Logging {

override protected val logger: Logger =
Logger(LoggerFactory getLogger getClass.getName)

   val timedRuns = ArrayBuffer[Double]()

   /**
    * Takes a function that we would like to measure the time to execute.
    * Internally, a buffer stores each time measured by execute. This is used by statistics methods
    * like mean, variance, standard deviation
    *
    * @param f function to be profiled
    * @return
    */
   def execute(f: => AnyRef): AnyRef =
   {
      val startTime = System.currentTimeMillis()
      val returnedVal = f
      val stopTime = System.currentTimeMillis()

      timedRuns += (stopTime - startTime)
      logger.info("Execution time: " + (stopTime - startTime) + "ms")

      returnedVal
   }

   def mean: Double =
      timedRuns.sum / timedRuns.size

   def variance: Double =
   {
      timedRuns
         .foldLeft(0.0)(
            (count, r) =>
               count + math.pow(r - mean, 2)) / timedRuns.size
   }

   def standardDeviation: Double =
   {
      math.sqrt(variance)
   }


   override def toString(): String =
   {
      val sb = new StringBuilder()
      sb.append("Avg. run time: ").append(mean).append(" ms").append("\n")
      sb.append("Var. run time: ").append(variance).append(" ms").append("\n")
      sb.append("Std. dev. run time: ").append(standardDeviation).append(" ms")

      sb.toString
   }


   def main(args: Array[String])
   {
      (0 until 10000000).foreach {
         i => {

         }
      }
   }
}

