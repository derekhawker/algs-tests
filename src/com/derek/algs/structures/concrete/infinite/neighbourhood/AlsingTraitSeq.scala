package com.derek.algs.structures.concrete.infinite.neighbourhood

import com.derek.algs.structures.specification.TraitSeq
import java.awt.Color
import scala.util.Random


class AlsingTraitSeq(val xs: Array[AlsingPolygon],
                     val width: Int,
                     val height: Int) extends TraitSeq[AlsingPolygon]  with Serializable{

  /**
   *
   * @return Deep copy of this trait (all levels)
   */
  override def deepcopy(): AlsingTraitSeq = {
    // Lots of problems creating a new array that doesn't become an ArraySeq. This works for now
    val deepArray = xs.slice(0, xs.length)

    deepArray.zipWithIndex
      .foreach(r =>
      deepArray(r._2) =

        new AlsingPolygon(
          r._1.vertices.clone(),
          new Color(r._1.colour.getRed, r._1.colour.getGreen, r._1.colour.getBlue,
            r._1.colour.getAlpha))
      )

    new AlsingTraitSeq(deepArray, width, height)
  }

  /**
   * Can't really compute a "best" neighbourhood move since the neighbouring solutions at this
   * particular move are infinite. Returns a random solution instead
   *
   * @param move
   * @param scorer
   * @return
   */
  override def bestNeighbourhoodMove(move: Int,
                                     scorer: (TraitSeq[AlsingPolygon]) => Double): (TraitSeq[AlsingPolygon], Double) = {
    val best = this.deepcopy()
    best(move) = randNeighbourhoodMove(move)

    (best, scorer(best))
  }

  override def randNeighbourhoodMove(move: Int): AlsingPolygon = {
    Random.nextInt(2) match {
      case 0 =>
        // Add/Remove a vertex.
        if (Random.nextBoolean()) {

          // If there aren't too many vertex, otherwise, delete a vertex.
          if (this(move).vertices.length < AlsingPolygon.MaxVertices) {
            AlsingPolygon.addVertex(this(move), width, height)
          } else {
            AlsingPolygon.removeVertex(this(move))
          }
        } else {

          // If there aren't too many vertex, otherwise, delete a vertex
          if (this(move).vertices.length > AlsingPolygon.MinVertices) {
            AlsingPolygon.removeVertex(this(move))
          } else {
            AlsingPolygon.addVertex(this(move), width, height)
          }
        }
      case 1 =>
        // Change color
        new AlsingPolygon(this(move).vertices.clone(),
          new Color(Random.nextInt(256), Random.nextInt(256), Random.nextInt(256),
            Random.nextInt(256)))
      case 2 =>
        val vs = Random.nextInt(4) + 3

        val vertices = Array.range(0, vs)
          .map(m =>
          (Random.nextInt(width),
            Random.nextInt(height)))

        val randColour = new Color(Random.nextInt(255), Random.nextInt(255), Random.nextInt(255),
          Random.nextInt(255))

        new AlsingPolygon(vertices,
          randColour)

    }
  }

  override def iterator: Iterator[AlsingPolygon] =
    xs.iterator

  override def update(index: Int, value: AlsingPolygon): Unit =
    xs(index) = value

  override def apply(index: Int): AlsingPolygon =
    xs(index)

  override def length: Int =
    xs.length

  override def toString(): String = {
    ""
  }
}

class AlsingPolygon(val vertices: Array[(Int, Int)], val colour: Color) extends Serializable

object AlsingPolygon {
  val MaxVertices = 10
  val MinVertices = 3

  def removeVertex(polygon: AlsingPolygon): AlsingPolygon = {
    val copy = new Array[(Int, Int)](polygon.vertices.length - 1)
    polygon.vertices.copyToArray(copy, 0)

    new AlsingPolygon(copy, polygon.colour)
  }


  def addVertex(polygon: AlsingPolygon, width: Int, height: Int): AlsingPolygon = {
    val copy = new Array[(Int, Int)](polygon.vertices.length + 1)

    polygon.vertices.copyToArray(copy, 0)
    copy(polygon.vertices.length) =
      (Random.nextInt(width), Random.nextInt(height))

    new AlsingPolygon(copy, polygon.colour)
  }
}