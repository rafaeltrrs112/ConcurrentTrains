package cross

import java.text.DecimalFormat

/**
  * Created by rtorres12 on 12/8/15.
  */
object HeatConstants {
  val EPSILON : Double = .01
  val UPPER_LEFT = "upperLeft"
  val UPPER_RIGHT = "upperRight"

  val LOWER_LEFT = "lowerLeft"
  val LOWER_RIGHT = "lowerRight"

  val CENTER_CELL = "centerCell"

  val LEFT_BORDER = "leftBorder"
  val RIGHT_BORDER = "rightBorder"
  val UPPER_BORDER = "topBorder"
  val LOWER_BORDER = "bottomBorder"

  val MAX_TEMPERATURE : Double = 10000.0
  val S_VALUE = MAX_TEMPERATURE
  val T_VALUE = 112
  val MAX_RATIO : Double = 255 / MAX_TEMPERATURE
  val formatter = new DecimalFormat("##")

  //SERVER : Tag for an init request, and an init receive
  val INIT_REQUEST = -30
  val INIT_DATA = -31

  //CLIENT received border tag
  val IMG_UPDATE = -32

  val BORDER_UPDATE = -33

}