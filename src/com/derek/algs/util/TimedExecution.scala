package com.derek.algs.util

import scala.collection.mutable.ArrayBuffer

/**
 * @author Derek Hawker
 */
object TimedExecution {
  val timedRuns = ArrayBuffer[Double]()

  def run(f: => AnyRef): AnyRef = {
    val startTime = System.currentTimeMillis()
    val returnedVal = f
    val stopTime = System.currentTimeMillis()

    timedRuns += (stopTime - startTime)
    println("Execution time: " + (stopTime - startTime) + "ms")

    returnedVal
  }
}
