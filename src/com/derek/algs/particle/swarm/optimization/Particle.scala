package com.derek.algs.particle.swarm.optimization

import com.derek.algs.structures.specification.TraitSeq

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

