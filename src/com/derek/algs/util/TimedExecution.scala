package com.derek.algs.util

import scala.collection.mutable.ArrayBuffer

/**
 * @author Derek Hawker
 */
class TimedExecution {
  val timedRuns = ArrayBuffer[Double]()

  def run(f: => AnyRef): AnyRef = {
    val startTime = System.currentTimeMillis()
    val returnedVal = f
    val stopTime = System.currentTimeMillis()

    timedRuns += (stopTime - startTime)
    println("Execution time: " + (stopTime - startTime) + "ms")

    returnedVal
  }

  def meanRunTime(): Double =
    timedRuns.foldLeft(0.0)(_ + _) / timedRuns.size

  def varianceRunTime(): Double = {
    val mean = meanRunTime()
    timedRuns.foldLeft(0.0)(
      (count, r) =>
        count + math.pow(r - mean, 2)) / timedRuns.size
  }
}
