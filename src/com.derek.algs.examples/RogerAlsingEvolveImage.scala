package com.derek.algs.examples

import java.awt._
import scala.util.Random
import com.derek.algs.structures.concrete.infinite.neighbourhood.{AlsingPolygon, AlsingTraitSeq}
import com.derek.algs.structures.specification.TraitSeq
import javax.imageio.ImageIO
import java.awt.image.{DataBufferInt, BufferedImage}
import java.io.File
import java.awt.geom.{GeneralPath, Path2D}
import com.derek.algs.util.{SerializeHelper, TimedExecution}
import com.derek.algs.{Tabusearch, GeneticAlgorithm}

/**
 * use genetic algorithms to evolve a set of polygons to resemble a given picture
 *
 * @author Derek Hawker
 */
object RogerAlsingEvolveImage {

  val (goalImagePixels, goalImageWidth, goalImageHeight) =
    convertGoalImage2Array(
      "/home/derekhawker/programs/scala/Algorithms/res/img/mona-lisa.png")

  def main(args: Array[String]) {

    val numPolygons = 100
    val rng = new Random()

    def population = Array.range(0, 4).map(

      p => {
        val polygons = Array.range(0, numPolygons).map(

          m => {
            val vs =  3

            val vertices = Array.range(0, vs)
              .map(m =>
              (0,0))

            val randColour = new Color(0, 0, 0, 0)

            new AlsingPolygon(vertices,
              randColour)
          })

        new AlsingTraitSeq(polygons, goalImageWidth, goalImageHeight)
          .asInstanceOf[TraitSeq[AlsingPolygon]]
      })


    var tb = Tabusearch.defaultArguments[AlsingPolygon](population.head, scorer)
    SerializeHelper.serializeToFile(tb, tb.filename)
    printer(population.head, "tboriginal.png")
    (0 until 30000).foreach(
      i => {
        tb = SerializeHelper.deserializeFromFile(tb.filename)
          .asInstanceOf[Tabusearch[AlsingPolygon]]
        new TimedExecution().execute {
          val best = tb.execute()
          printer(best, "tbfinal%07d.png".format(i))
          SerializeHelper.serializeToFile(tb, tb.filename)

          best
        }
      })
  }


  def convertGoalImage2Array(filename: String): (Array[Int], Int, Int) = {
    val goalImg = ImageIO.read(
      new File(filename))
    val bImg = new BufferedImage(goalImg.getWidth, goalImg.getHeight, BufferedImage.TYPE_INT_RGB)
    val g2 = bImg.createGraphics()

    // Going to disable AA for now
    //g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

    // draw the image
    g2.drawImage(goalImg, 0, 0, null)

    // Extract the pixels (used for comparing to traits
    val pixels = bImg.getRaster.getDataBuffer.asInstanceOf[DataBufferInt].getData()

    (pixels, bImg.getWidth, bImg.getHeight)
  }

  def printer[T](best: TraitSeq[T], filename: String): Unit = {
    // 1. Draw the trait to a bufferedImg
    val bufferedImg = new BufferedImage(goalImageWidth, goalImageHeight, BufferedImage.TYPE_INT_RGB)
    val g2 = bufferedImg.createGraphics()
    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

    g2.setBackground(new Color(0, 0, 0))
    g2.clearRect(0, 0, goalImageHeight, goalImageHeight)
    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

    val ts = best.asInstanceOf[AlsingTraitSeq]
    ts.foreach(
      polygon =>
        paintPolygon(g2, polygon.vertices, polygon.colour)
    )


    saveBufferedImage(bufferedImg, filename)
  }

  def saveBufferedImage(bufferedImage: BufferedImage, filename: String): Unit = {
    val f = new File("C:\\Users\\derekhawker\\programs\\scala\\Algorithms\\" + filename)

    if (f.exists())
      System.out.println("exiss")
    else
      System.out.println("doesn't exists")

    if (ImageIO.write(bufferedImage, "png", f)) {
      System.out.println("-- saved")
    }
  }

  /**
   * Computes a score for the seq of polygons based on how closely they match an image
   *
   * @param traitsequence
   * @tparam T
   * @return
   */
  def scorer[T](traitsequence: TraitSeq[T]): Double = {
    val ts = traitsequence.asInstanceOf[AlsingTraitSeq]

    // 1. Draw the trait to a bufferedImg
    val bufferedImg = new BufferedImage(goalImageWidth, goalImageHeight, BufferedImage.TYPE_INT_RGB)
    val g2 = bufferedImg.createGraphics()
    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

    g2.setBackground(new Color(0, 0, 0))
    g2.clearRect(0, 0, goalImageHeight, goalImageHeight)
    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

    ts.foreach(
      polygon =>
        paintPolygon(g2, polygon.vertices, polygon.colour)
    )

    //saveBufferedImage(bufferedImg)

    // 2. Extract pixels.
    val pixels = bufferedImg.getRaster.getDataBuffer.asInstanceOf[DataBufferInt].getData()

    // 3. compare to goal image by summing the total distance to rgb pixels
    val pixelDifference = goalImagePixels.zip(pixels).foldLeft(0.0)(
      (difference, pair) => {
        difference + math.abs(pair._1 - pair._2)
      })

    // invert result since the more difference there is the worse we want the score to be
    -pixelDifference
  }


  def paintPolygon(g2: Graphics2D, vertices: Array[(Int, Int)], paint: Paint): Unit = {
    // fill and stroke GeneralPath
    val filledPolygon = new GeneralPath(Path2D.WIND_EVEN_ODD, vertices.length)
    filledPolygon.moveTo(vertices.head._1, vertices.head._2)

    vertices.tail.foreach(
      vert => filledPolygon.lineTo(vert._1, vert._2)
    )
    filledPolygon.closePath()

    g2.setPaint(paint)
    g2.fill(filledPolygon)
  }

}
