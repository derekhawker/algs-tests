package com.derek.algs.examples

import scala.reflect.io.File
import java.awt.Color
import scala.util.Random
import com.derek.algs.structures.concrete.infinite.neighbourhood.TraitSeqRef

/**
 * use genetic algorithms to evolve a set of polygons to resemble a given picture
 *
 * @author Derek Hawker
 */
object RogerAlsingEvolveImage {
  def main(args: Array[String]) {
    // If no image provided. Check for default, else throw exception
    if (args.length == 0 || !File("mona-lisa.png").exists)
      throw new RuntimeException("no input picture file")

    val (maxWidth, maxHeight) = (100, 100)
    val numPolygons = 50
    val rng = new Random()

    def population = Array.range(0, 10).map(
      p => {
        val polygons = Array.range(0, numPolygons).map(
          m => {
            val vs = rng.nextInt(4) + 3

            val vertices = Array.range(0, vs)
              .map(m =>
              (rng.nextInt(maxWidth),
                rng.nextInt(maxHeight)))

            new AlsingPolygon(vertices,
              new Color(rng.nextInt(255), rng.nextInt(255), rng.nextInt(255), rng.nextInt(255)))
          })

        new TraitSeqRef[AlsingPolygon](polygons,
          (t: AlsingPolygon) =>
            new AlsingPolygon(t.vertices.clone(), t.color))
      })
  }
}

class AlsingPolygon(val vertices: Array[(Int, Int)], val color: Color) {

}
