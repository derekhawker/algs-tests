
package meta_heuristics.util.gif

import java.awt.image.BufferedImage
import meta_heuristics.structures.specification.TraitSeq
import meta_heuristics.structures.concrete.finite.neighbourhood.TraitSeqVal

/**
 * @author Derek Hawker
 */
class AnimatedProgressGif(filename: String)
{
  val scaleFactor = 4
  val width       = 17 * scaleFactor

  private val totalPixels = 17 * scaleFactor * 17 * scaleFactor * 3

  val rawE  = new AnimatedGifEncoder
  val greyE = new AnimatedGifEncoder
  rawE.start("raw-" + filename)
  greyE.start("grey-" + filename)
  greyE.setDelay(100)
  rawE.setDelay(100)
  greyE.setRepeat(0)
  greyE.setRepeat(0)

  def addFrame(traitsequence: TraitSeq[Int]): Unit =
  {
    val greypixels = new Array[Int](totalPixels)
    val rawpixels = new Array[Int](totalPixels)

    /*
    This is ugly but intended to create scaled 17x17 image. All the extra loops are used to repeat
    pixels. The first (0 until scalefactor), repeats a written row a number of times. The second
    (0 until scalefactor), repeats a column a number of times.
      */
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
            (0 until 3) // each byte of rgb must be written indiviudally. Thus 3
              .foreach(rgb => {
              rawpixels((row * scaleFactor + repeatRow) * (17 * scaleFactor * 3)
                + ((col * scaleFactor * 3) + (i * 3) + rgb)) =
                traitsequence(index) match {
                  case 0 => Array(225, 15, 15)(rgb) // Red
                  case 1 => Array(0, 205, 15)(rgb) // Green
                  case 2 => Array(0, 25, 225)(rgb) // Blue
                  case 3 => Array(225, 225, 15)(rgb) // Yellow
                }
              if (isViolation) {
                greypixels((row * scaleFactor + repeatRow) * (17 * scaleFactor * 3)
                  + ((col * scaleFactor * 3) + (i * 3) + rgb)) =
                  traitsequence(index) match {
                    case 0 => Array(225, 15, 15)(rgb) // Red
                    case 1 => Array(0, 205, 15)(rgb) // Green
                    case 2 => Array(0, 25, 225)(rgb) // Blue
                    case 3 => Array(225, 225, 15)(rgb) // Yellow
                  }
              } else {
                greypixels((row * scaleFactor + repeatRow) * (17 * scaleFactor * 3)
                  + ((col * scaleFactor * 3) + (i * 3) + rgb)) =
                  traitsequence(index) match {
                    case 0 => Array(185, 185, 185)(rgb)
                    case 1 => Array(140, 140, 140)(rgb)
                    case 2 => Array(115, 115, 115)(rgb)
                    case 3 => Array(85, 85, 85)(rgb)

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


  def finish(): Unit =
  {
    rawE.finish()
    greyE.finish()

  }


  private def hasSameColourRectangles(traitsequence: TraitSeq[Int], starty: Int,
                                      startx: Int): Boolean =
  {
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


object AnimatedProgressGif
{
  var currentGif: AnimatedProgressGif = null


  def main(args: Array[String])
  {
    val xs = "0332233001212320202220221301331121133112010011320333313311010022033212312312230012231313102320122323303000100130231320000031330120202230230202011203322322012020333111210011130012032033202120113122110330300030122322011003303221103302210110003232122132132112311231103202330320112300013223011"
      .map(
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