package com.derek.algs.structures

import scala.collection.mutable
import scala.util.Random

/**
 * @author Derek Hawker
 */
trait TraitSequence[T] {
  def bestNeighbourhoodMove(move: Int,
                            scorer: (TraitSequence[T]) => Double): (TraitSequence[T], Double)

  def randNeighbourhoodMove(move: Int, rng: Random): T

  def deepcopy(): TraitSequence[T]

  def length: Int

  def apply(index: Int): T
  def update(index: Int, value: T): Unit
}

