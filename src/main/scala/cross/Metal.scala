package cross

/**
  * Created by rtorres12 on 12/8/15.
  */
trait Metal{
  var percentage : Double
  val thermalConstant : Double
}

case class Adamantium(override var percentage : Double) extends Metal {
  override val thermalConstant : Double = 0.75
}

case class Vibranium(override var percentage : Double) extends Metal{
  override val thermalConstant : Double = 1.0
}

case class Chromium(override var percentage : Double) extends Metal {
  override val thermalConstant : Double = 1.25
}
