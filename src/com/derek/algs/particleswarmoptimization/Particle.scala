package com.derek.algs.particleswarmoptimization

import com.derek.algs.structures.TraitSeq

/**
 *
 * @author Derek Hawker
 *
 * @param position
 * @param velocity
 * @tparam T
 */
class Particle[T](val position: TraitSeq[T],
                             val velocity: TraitSeq[T],
                              val localBest: TraitSeq[T])

