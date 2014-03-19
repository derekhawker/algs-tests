
package com.derek.algs.util.gif

import java.awt.image.BufferedImage
import com.derek.algs.structures.specification.TraitSeq
import com.derek.algs.structures.concrete.finite.neighbourhood.TraitSeqVal

/**
 * @author Derek Hawker
 */
class AnimatedProgressGif(filename: String) {
  val scaleFactor = 4
  val width       = 17 * scaleFactor

  private val totalPixels = 17 * scaleFactor * 17 * scaleFactor * 3

  val rawE  = new AnimatedGifEncoder
  val greyE = new AnimatedGifEncoder
  rawE.start("raw-" + filename)
  greyE.start("grey-" + filename)
  rawE.setDelay(100)
  greyE.setRepeat(0)

  def addFrame(traitsequence: TraitSeq[Int]): Unit = {
    val greypixels = new Array[Int](totalPixels)
    val rawpixels = new Array[Int](totalPixels)

    (0 until 17)
      .foreach(row => {
      (0 until 17)
        .foreach(col => {
        val index = row * 17 + col
        val isViolation = hasSameColourRectangles(traitsequence, row, col)
        (0 until scaleFactor)
          .foreach(repeatRow =>
          (0 until scaleFactor)
            .foreach(i => {
            (0 until 3)
              .foreach(rgb => {
              rawpixels((row * scaleFactor + repeatRow) * (17 * scaleFactor * 3)
                + ((col * scaleFactor * 3) + (i * 3) + rgb)) =
                traitsequence(index) match {
                  case 0 => Array(255, 0, 0)(rgb)
                  case 1 => Array(0, 255, 0)(rgb)
                  case 2 => Array(0, 0, 255)(rgb)
                  case 3 => Array(255, 255, 0)(rgb)
                }
              if (isViolation) {
                greypixels((row * scaleFactor + repeatRow) * (17 * scaleFactor * 3)
                  + ((col * scaleFactor * 3) + (i * 3) + rgb)) =
                  traitsequence(index) match {
                    case 0 => Array(255, 0, 0)(rgb)
                    case 1 => Array(0, 255, 0)(rgb)
                    case 2 => Array(0, 0, 255)(rgb)
                    case 3 => Array(255, 255, 0)(rgb)
                  }
              } else {
                greypixels((row * scaleFactor + repeatRow) * (17 * scaleFactor * 3)
                  + ((col * scaleFactor * 3) + (i * 3) + rgb)) =
                  traitsequence(index) match {
                    case 0 => Array(225, 225, 225)(rgb)
                    case 1 => Array(205, 205, 200)(rgb)
                    case 2 => Array(175, 175, 175)(rgb)
                    case 3 => Array(145, 145, 145)(rgb)

                  }
              }
            })
          })
          )
      })
    })


    List((rawE, rawpixels), (greyE, greypixels))
      .foreach(pair => {
      val image = new BufferedImage(width, width, BufferedImage.TYPE_INT_RGB)
      var raster = image.getRaster
      raster.setPixels(0, 0, width, width, pair._2)

      pair._1.addFrame(image)
    })
  }


  def finish(): Unit = {
    rawE.finish()
    greyE.finish()

  }


  private def hasSameColourRectangles(traitsequence: TraitSeq[Int], starty: Int,
                                      startx: Int): Boolean = {
    val ul = starty * 17 + startx

    (0 until 17).foreach(
      row => {
        val bl = row * 17 + startx

        // Only continue if upper-left and bottom-left corners are same colour
        if (traitsequence(bl) == traitsequence(ul)) {
          (0 until 17).foreach(
            col => {
              if (row != starty && col != startx) {
                val ur = starty * 17 + col
                val br = row * 17 + col

                // Now check if the upper-right and bottom-right corner have the same colour as others.
                if (traitsequence(ul) != traitsequence(ur)
                  || traitsequence(ul) != traitsequence(br)) {
                } else {
                  return true
                }
              }
            })
        }
      })

    false
  }
}


object AnimatedProgressGif {
  var currentGif: AnimatedProgressGif = null


  def main(args: Array[String]) {
    val xs = "0332233001212320202220221301331121133112010011320333313311010022033212312312230012231313102320122323303000100130231320000031330120202230230202011203322322012020333111210011130012032033202120113122110330300030122322011003303221103302210110003232122132132112311231103202330320112300013223011".map(
      _.toInt - 48).toArray
    val ts = new TraitSeqVal(xs, null)
    val gif = new AnimatedProgressGif("temp.gif")
    gif.addFrame(ts)
    gif.finish()

  }

  def apply(filename: String): Unit =
    currentGif = new AnimatedProgressGif(filename)

  def apply =
    currentGif
}