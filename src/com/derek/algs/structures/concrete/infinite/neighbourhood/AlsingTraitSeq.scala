package com.derek.algs.structures.concrete.infinite.neighbourhood

import com.derek.algs.structures.specification.TraitSeq
import scala.util.Random
import java.awt.Color


class AlsingTraitSeq(val xs: Array[AlsingPolygon],
                     val width: Int,
                     val height: Int) extends TraitSeq[AlsingPolygon] with Serializable {

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
    val startScore: Double = scorer(this)

    val bestMove = (0 until 1).foldLeft((this, startScore))(

      (bestAttempt, n) => {
        val curr = this.deepcopy()

        curr(move) = randNeighbourhoodMove(move)
        val score = scorer(curr)

        if (score > startScore)
          (curr, score)
        else
          bestAttempt
      })

    bestMove
  }

  override def randNeighbourhoodMove(move: Int): AlsingPolygon = {

    Random.nextInt(7) match {
      case 0 =>
        val vs = Random.nextInt(4) + 3

        val vertices = Array.range(0, vs)
          .map(m =>
          (Random.nextInt(width), Random.nextInt(height)))

        new AlsingPolygon(vertices, this(move).colour)
      case 1 =>
        val vs = Random.nextInt(4) + 3

        val vertices = Array.range(0, vs)
          .map(m =>
          (Random.nextInt(width), Random.nextInt(height)))

        val randColour = new Color(Random.nextInt(255), Random.nextInt(255), Random.nextInt(255),
          Random.nextInt(255))

        new AlsingPolygon(vertices, randColour)
      case 2 =>

        new AlsingPolygon(this(move).vertices.clone(),
          new Color(this(move).colour.getRed, this(move).colour.getGreen, this(move).colour.getBlue,
            Random.nextInt(48)))
      case 3 =>
        val minMax = this(move).vertices.foldLeft((width, height, 0, 0))(
          (thresholds, v) => {
            val minx = thresholds._1
            val miny = thresholds._2
            val maxx = thresholds._3
            val maxy = thresholds._4

            (math.min(minx, v._1),
              math.min(miny, v._2),
              math.max(maxx, v._1),
              math.max(maxy, v._2))
          })

        if (Random.nextBoolean()) {
          if (Random.nextBoolean() && minMax._1 > 0) {
            val xo = Random.nextInt(minMax._1 )
            new AlsingPolygon(this(move).vertices.map(v => (v._1 - xo, v._2)),
              this(move).colour)
          } else if (minMax._2 > 0){
            val yo = Random.nextInt(minMax._2 )
            new AlsingPolygon(this(move).vertices.map(v => (v._1, v._2 - yo)),
              this(move).colour)
          } else{
            this(move)
          }
        } else {
          if (Random.nextBoolean() && minMax._3 < width) {
            val xo = Random.nextInt(width - minMax._3)
            new AlsingPolygon(this(move).vertices.map(v => (v._1 + xo, v._2)),
              this(move).colour)
          } else if(minMax._4 < height) {
            val yo = Random.nextInt(height - minMax._4)
            new AlsingPolygon(this(move).vertices.map(v => (v._1, v._2 + yo)),
              this(move).colour)
          }else{
            this(move)
          }
        }
      case 4 =>

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
      case 5 =>
        val rVertex = Random.nextInt(this(move).vertices.length)

        val vertices = this(move).vertices.zipWithIndex
        .map( v=> v._2 match {
          case n if n == rVertex =>
            (Random.nextInt(width), Random.nextInt(height))
          case _ =>
            v._1
        })

        new AlsingPolygon(vertices, this(move).colour)

      case 6 =>
        new AlsingPolygon(this(move).vertices.clone(),
          new Color(this(move).colour.getRed, this(move).colour.getGreen, this(move).colour.getBlue,
            0))
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
    polygon.vertices.tail.copyToArray(copy, 0)

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