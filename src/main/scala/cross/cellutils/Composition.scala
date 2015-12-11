package cross.cellutils

import cross.{Chromium, Vibranium, Adamantium}

import scala.util.Random

/**
  * Created by rtorres12 on 12/8/15.
  */
case class Composition(var adamantium : Adamantium, var vibranium: Vibranium, var chromium : Chromium)

object Composition {
  def randomComposition : Composition = {
    val firstRandom : Double = Random.nextDouble * 100
    val secondRandom : Double = Random.nextDouble * 100
    val thirdRandom : Double = Random.nextDouble * 100

    val total : Double = firstRandom + secondRandom + thirdRandom

    Composition(Adamantium(firstRandom / total), Vibranium(secondRandom / total), Chromium(thirdRandom / total))
  }
}
