//package com.derek.algs.examples
//
//import java.awt._
//import scala.util.Random
//import meta_heuristics.structures.concrete.infinite.neighbourhood.{INTraitSeqRef, AlsingPolygon}
//import meta_heuristics.structures.specification.TraitSeq
//import javax.imageio.ImageIO
//import java.awt.image.{DataBufferInt, BufferedImage}
//import java.io.File
//import java.awt.geom.{GeneralPath, Path2D}
//import meta_heuristics.util.{SerializeHelper, TimedExecution}
//import com.derek.algs.{ParticleSwarm, Tabusearch, GeneticAlgorithm}
//import meta_heuristics.particle_swarm_optimization.particle.Particle
//
///**
// * use genetic algorithms to evolve a set of polygons to resemble a given picture
// *
// * @author Derek Hawker
// */
//object RogerAlsingEvolveImage
//{
//
//  class Polygon(val vertices: Array[(Double, Double)],
//                val colour: (Double, Double, Double, Double)) extends Serializable
//
//  val (goalImagePixels, goalImageWidth, goalImageHeight) =
//    convertGoalImage2Array(
//      "/home/derekhawker/programs/scala/Algorithms/res/img/mona-lisa.png")
//
//  def main(args: Array[String])
//  {
//
//    val numPolygons = 30
//    val numPopulation = 20
//
//    val positionPopulation: Array[TraitSeq[RogerAlsingEvolveImage.Polygon]] =
//      generateRandomPositionPopulation(numPopulation, numPolygons, goalImageWidth, 0,
//        goalImageHeight, 0,
//        255, 0)
//
//    val velocityPopulation =
//      generateRandomVelocityPopulation(positionPopulaition, numPopulation, numPolygons,
//        goalImageWidth, 0, goalImageHeight, 0, 255, 0)
//
//    val population = Array.range(0, numPopulation)
//      .map(i =>
//      new Particle[Polygon](positionPopulation(i), velocityPopulation(i), positionPopulation(i),
//        null))
//
//
//    val lowerBounds =
//      generateBounds(numPolygons, 0, 0, 0)
//    val upperBounds =
//      generateBounds(numPolygons, goalImageWidth, goalImageHeight, 255)
//
//    val positionBounds = lowerBounds.zip(upperBounds).toArray
//
//    var tb = ParticleSwarm.defaultArguments[Polygon](population, positionBounds, updateVelocity,
//      updatePosition, scorer)
//    SerializeHelper.serializeToFile(tb, tb.filename)
//    printer(population.head.position, "pso.png")
//    (0 until 30000).foreach(
//      i =>
//      {
//        tb = SerializeHelper.deserializeFromFile(tb.filename)
//          .asInstanceOf[ParticleSwarm[Polygon]]
//        new TimedExecution().execute
//        {
//          val best = tb.execute()
//          printer(best, "pso%07d.png".format(i))
//          SerializeHelper.serializeToFile(tb, tb.filename)
//
//          best
//        }
//      })
//  }
//
//  private def updateVelocity(position: Polygon,
//                             velocity: Polygon,
//                             localBestPosition: Polygon,
//                             globalBestPosition: Polygon,
//                             velocityFollow: Double,
//                             globalOptimumFollow: Double,
//                             localOptimumFollow: Double): Polygon =
//  {
//
//    val posVerts = position.vertices
//    val velVerts = velocity.vertices
//    val gbVerts = globalBestPosition.vertices
//    val lbVerts = localBestPosition.vertices
//
//    def updateVertexVelocity(i: Int,
//                             n: Int): Double =
//      (velocityFollow * velVerts(i).productElement(n).asInstanceOf[Double]
//        + globalOptimumFollow * Random.nextDouble() * (gbVerts(i).productElement(n)
//        .asInstanceOf[Double] - posVerts(i).productElement(n).asInstanceOf[Double])
//        + localOptimumFollow * Random.nextDouble() * (lbVerts(i).productElement(n)
//        .asInstanceOf[Double] - posVerts(i).productElement(n).asInstanceOf[Double]))
//
//    val updatedVertexVelocities = Array.range(0, posVerts.length)
//      .map(i =>
//    {
//      (updateVertexVelocity(i, 0), updateVertexVelocity(i, 1))
//    })
//
//
//
//    val posColour = position.colour
//    val velColour = velocity.colour
//    val gbColour = globalBestPosition.colour
//    val lbColour = localBestPosition.colour
//
//    def updateColourVelocity(i: Int): Double =
//      (velocityFollow * velColour.productElement(i).asInstanceOf[Double]
//        + globalOptimumFollow * Random.nextDouble() * (gbColour.productElement(i)
//        .asInstanceOf[Double] - posColour.productElement(i).asInstanceOf[Double])
//        + localOptimumFollow * Random.nextDouble() * (lbColour.productElement(i)
//        .asInstanceOf[Double] - posColour.productElement(i).asInstanceOf[Double]))
//
//
//    val updatedColourVelocities = (updateColourVelocity(0), updateColourVelocity(1),
//      updateColourVelocity(2), updateColourVelocity(3))
//
//
//    new Polygon(updatedVertexVelocities, updatedColourVelocities)
//  }
//
//  private def updatePosition(position: Polygon,
//                             velocity: Polygon,
//                             positionBounds: (Polygon, Polygon)): Polygon =
//  {
//
//    val posVerts = position.vertices
//    val velVerts = velocity.vertices
//    val posLb = positionBounds._1.vertices
//    val posUb = positionBounds._2.vertices
//
//    def updateVertexPosition(i: Int,
//                             n: Int): Double =
//    {
//      val newpos = (((posVerts(i).productElement(n).asInstanceOf[Double] + velVerts(i)
//        .productElement(n)
//        .asInstanceOf[Double]) % posUb(i).productElement(n).asInstanceOf[Double])
//        + posLb(i).productElement(n).asInstanceOf[Double])
//
//      if (newpos < 0)
//        newpos + posUb(i).productElement(n).asInstanceOf[Double]
//      else
//        newpos
//    }
//
//    val updatedVertexPositions = Array.range(0, posVerts.length)
//      .map(i =>
//      (updateVertexPosition(i, 0), updateVertexPosition(i, 1))
//      )
//
//    val posColour = position.colour
//    val velColour = velocity.colour
//    val colourLb = positionBounds._1.colour
//    val colourUb = positionBounds._2.colour
//
//    def updateColourPosition(i: Int): Double =
//    {
//      val newpos = (((posColour.productElement(i).asInstanceOf[Double] + velColour.productElement(i)
//        .asInstanceOf[Double]) % colourUb.productElement(i).asInstanceOf[Double])
//        + colourLb.productElement(i).asInstanceOf[Double])
//
//      if (newpos < 0)
//        newpos + colourUb.productElement(i).asInstanceOf[Double]
//      else
//        newpos
//    }
//
//
//    val updatedColourPositions = (updateColourPosition(0), updateColourPosition(1),
//      updateColourPosition(2), updateColourPosition(3))
//
//    new Polygon(updatedVertexPositions, updatedColourPositions)
//  }
//
//
//  private def generateRandomPositionPopulation(numPopulation: Int,
//                                               numPolygons: Int,
//                                               lbXVertex: Double,
//                                               ubXVertex: Double,
//                                               lbYVertex: Double,
//                                               ubYVertex: Double,
//                                               lbColour: Double,
//                                               ubColour: Double): Array[TraitSeq[Polygon]] =
//  {
//
//    Array.range(0, numPopulation).map(
//      p =>
//      {
//        val polygons = Array.range(0, numPolygons).map(
//          m =>
//          {
//            val vertices = Array.range(0, 4)
//              .map(i =>
//              (Random.nextDouble() * ubXVertex + lbXVertex,
//                Random.nextDouble() * ubYVertex + lbYVertex)
//              )
//
//            val colour = (Random.nextDouble() * ubColour + lbColour,
//              Random.nextDouble() * ubColour + lbColour,
//              Random.nextDouble() * ubColour + lbColour,
//              Random.nextDouble() * ubColour + lbColour)
//
//            new Polygon(vertices, colour)
//          }
//        )
//
//        new INTraitSeqRef[Polygon](polygons,
//          (poly: Polygon) => new Polygon(poly.vertices.clone(), poly.colour))
//          .asInstanceOf[TraitSeq[Polygon]]
//      })
//  }
//
//
//  private def generateRandomVelocityPopulation(positionPolygons: Array[TraitSeq[Polygon]],
//                                                numPopulation: Int,
//                                               numPolygons: Int,
//                                               lbXVertex: Double,
//                                               ubXVertex: Double,
//                                               lbYVertex: Double,
//                                               ubYVertex: Double,
//                                               lbColour: Double,
//                                               ubColour: Double): Array[TraitSeq[Polygon]] =
//  {
//
//
//    Array.range(0, numPopulation).map(
//      p =>
//      {
//        val polygons = Array.range(0, numPolygons).map(
//          m =>
//          {
//            val vertices = Array.range(0, 4)
//              .map(i =>
//              (Random.nextDouble() * ubXVertex + lbXVertex,
//                Random.nextDouble() * ubYVertex + lbYVertex)
//              )
//
//            val colour = (Random.nextDouble() * ubColour + lbColour,
//              Random.nextDouble() * ubColour + lbColour,
//              Random.nextDouble() * ubColour + lbColour,
//              Random.nextDouble() * ubColour + lbColour)
//
//            new Polygon(vertices, colour)
//          }
//        )
//
//        new INTraitSeqRef[Polygon](polygons,
//          (poly: Polygon) => new Polygon(poly.vertices.clone(), poly.colour))
//          .asInstanceOf[TraitSeq[Polygon]]
//      })
//  }
//
//  private def generateBounds(numPolygons: Int,
//                             ubXVertex: Double,
//                             ubYVertex: Double,
//                             ubColour: Double): TraitSeq[RogerAlsingEvolveImage.Polygon] =
//  {
//
//    val polygons = Array.range(0, numPolygons).map(
//
//      m =>
//      {
//        val vertices = Array.range(0, 4)
//          .map(v =>
//          (ubXVertex, ubYVertex)
//          )
//
//
//        val colour = (Random.nextDouble() * ubColour,
//          Random.nextDouble() * ubColour,
//          Random.nextDouble() * ubColour,
//          Random.nextDouble() * ubColour)
//
//        new Polygon(vertices, colour)
//      }
//    )
//
//    new INTraitSeqRef[Polygon](polygons,
//      (poly: Polygon) => new Polygon(poly.vertices.clone(), poly.colour))
//      .asInstanceOf[TraitSeq[Polygon]]
//  }
//
//
//  private def convertGoalImage2Array(filename: String): (Array[Int], Int, Int) =
//  {
//    val goalImg = ImageIO.read(
//      new File(filename))
//    val bImg = new BufferedImage(goalImg.getWidth, goalImg.getHeight, BufferedImage.TYPE_INT_RGB)
//    val g2 = bImg.createGraphics()
//
//    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
//
//    // draw the image
//    g2.drawImage(goalImg, 0, 0, null)
//
//    // Extract the pixels (used for comparing to traits
//    val pixels = bImg.getRaster.getDataBuffer.asInstanceOf[DataBufferInt].getData()
//
//    (pixels, bImg.getWidth, bImg.getHeight)
//  }
//
//  private def printer[T](best: TraitSeq[T],
//                         filename: String): Unit =
//  {
//    // 1. Draw the trait to a bufferedImg
//    val bufferedImg = new BufferedImage(goalImageWidth, goalImageHeight, BufferedImage.TYPE_INT_RGB)
//    val g2 = bufferedImg.createGraphics()
//
//
//    g2.setBackground(new Color(0, 0, 0))
//    g2.clearRect(0, 0, goalImageHeight, goalImageHeight)
//    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
//
//
//    best.foreach(ts =>
//    {
//      val poly = ts.asInstanceOf[Polygon]
//      paintPolygon(g2, poly.vertices, new Color(poly.colour._1.toInt, poly.colour._2.toInt,
//        poly.colour._3.toInt, poly.colour._4.toInt))
//    })
//
//
//    saveBufferedImage(bufferedImg, filename)
//  }
//
//  def saveBufferedImage(bufferedImage: BufferedImage,
//                        filename: String): Unit =
//  {
//    val f = new File("C:\\Users\\derekhawker\\programs\\scala\\Algorithms\\" + filename)
//
//    if (f.exists())
//      System.out.logger.info("exiss")
//    else
//      System.out.logger.info("doesn't exists")
//
//    if (ImageIO.write(bufferedImage, "png", f))
//    {
//      System.out.logger.info("-- saved")
//    }
//  }
//
//  /**
//   * Computes a score for the seq of polygons based on how closely they match an image
//   *
//   * @param traitsequence
//   * @tparam T
//   * @return
//   */
//  def scorer[T](traitsequence: TraitSeq[T]): Double =
//  {
//    val ts = traitsequence.asInstanceOf[INTraitSeqRef[Polygon]]
//
//    // 1. Draw the trait to a bufferedImg
//    val bufferedImg = new BufferedImage(goalImageWidth, goalImageHeight, BufferedImage.TYPE_INT_RGB)
//    val g2 = bufferedImg.createGraphics()
//
//
//    g2.setBackground(new Color(0, 0, 0))
//    g2.clearRect(0, 0, goalImageHeight, goalImageHeight)
//    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
//
//
//    traitsequence.foreach(
//      ts =>
//      {
//        val poly = ts.asInstanceOf[Polygon]
//        paintPolygon(g2, poly.vertices, new Color(poly.colour._1.toInt, poly.colour._2.toInt,
//          poly.colour._3.toInt, poly.colour._4.toInt))
//      })
//
//
//
//    // 2. Extract pixels.
//    val pixels = bufferedImg.getRaster.getDataBuffer.asInstanceOf[DataBufferInt].getData()
//
//    // 3. compare to goal image by summing the total distance to rgb pixels
//    val pixelDifference = goalImagePixels.zip(pixels).foldLeft(0.0)(
//      (difference,
//       pair) =>
//      {
//        difference + math.abs(pair._1 - pair._2)
//      })
//
//    // invert result since the more difference there is the worse we want the score to be
//    -pixelDifference
//  }
//
//
//  def paintPolygon(g2: Graphics2D,
//                   vertices: Array[(Double, Double)],
//                   paint: Paint): Unit =
//  {
//    // fill and stroke GeneralPath
//    val filledPolygon = new GeneralPath(Path2D.WIND_EVEN_ODD, vertices.length)
//    filledPolygon.moveTo(vertices.head._1.toInt, vertices.head._2.toInt)
//
//    vertices.tail.foreach(
//      vert => filledPolygon.lineTo(vert._1, vert._2)
//    )
//    filledPolygon.closePath()
//
//    g2.setPaint(paint)
//    g2.fill(filledPolygon)
//  }
//
//}
