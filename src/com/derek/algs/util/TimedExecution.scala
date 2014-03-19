package com.derek.algs.util

import scala.collection.mutable.ArrayBuffer

/**
 * @author Derek Hawker
 */
class TimedExecution {
  val timedRuns = ArrayBuffer[Double]()

  def execute(f: => AnyRef): AnyRef = {
    val startTime = System.currentTimeMillis()
    val returnedVal = f
    val stopTime = System.currentTimeMillis()

    timedRuns += (stopTime - startTime)
    println("Execution time: " + (stopTime - startTime) + "ms")

    returnedVal
  }

  def meanRunTime: Double =
    timedRuns.sum / timedRuns.size

  def varianceRunTime: Double = {
    val mean = meanRunTime
    timedRuns
      .foldLeft(0.0)(
      (count, r) =>
        count + math.pow(r - mean, 2)) / timedRuns.size
  }


  override def toString(): String = {
    val sb = new StringBuilder()
    sb.append("Avg. run time: ").append(meanRunTime).append(" ms").append("\n")
    sb.append("Var. run time: ").append(varianceRunTime).append(" ms")

    sb.toString
  }


  def main(args: Array[String]) {
    (0 until 10000000).foreach{
      i => {
        
      }
    }
  }
}

