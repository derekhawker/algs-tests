package com.derek.algs

import com.derek.algs.structures.TraitSeq

/**
 * @author Derek Hawker
 */
trait ExecutableAlgorithm[T] {
  def execute(): TraitSeq[T]
}
