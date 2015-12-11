package cross

/**
  * Metal trait for holding alloy metallic data.
  */
trait Metal{
  var percentage : Double
  val thermalConstant : Double
}

/**
  * Adamantium metallic allow data container object.
  *
  * @param percentage
  *                   The percentage of this metal in the region it is
  *                   pertains to.
  */
case class Adamantium(override var percentage : Double) extends Metal {
  override val thermalConstant : Double = 0.75
}

/**
  * Vibranium metallic allow data container object.
  *
  * @param percentage
  *                   The percentage of this metal in the region it is
  *                   pertains to.
  */
case class Vibranium(override var percentage : Double) extends Metal{
  override val thermalConstant : Double = 1.0
}

/**
  * Chromium metallic allow data container object.
  *
  * @param percentage
  *                   The percentage of this metal in the region it is
  *                   pertains to.
  */
case class Chromium(override var percentage : Double) extends Metal {
  override val thermalConstant : Double = 1.25
}
