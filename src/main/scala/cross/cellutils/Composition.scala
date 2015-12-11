package cross.cellutils

import cross.{Chromium, Vibranium, Adamantium}
import scala.util.Random

/**
  * A cell's metallic composition.
  * @param adamantium
  *                   The adamantium in the cell.
  * @param vibranium
  *                  The vibranium in the cell.
  * @param chromium
  *                 The chromium in the cell.
  */
case class Composition(var adamantium : Adamantium, var vibranium: Vibranium, var chromium : Chromium)

object Composition {
  /**
    * Generates a random composition.
    * @return
    *         A random composition.
    */
  def randomComposition : Composition = {
    val firstRandom : Double = Random.nextDouble * 100
    val secondRandom : Double = Random.nextDouble * 100
    val thirdRandom : Double = Random.nextDouble * 100

    val total : Double = firstRandom + secondRandom + thirdRandom

    Composition(Adamantium(firstRandom / total), Vibranium(secondRandom / total), Chromium(thirdRandom / total))
  }
}
