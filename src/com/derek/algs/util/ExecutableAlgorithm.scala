package com.derek.algs.util

import com.derek.algs.structures.specification.TraitSeq

/**
 * @author Derek Hawker
 */
trait ExecutableAlgorithm[T] {
  def execute(): TraitSeq[T]
}
