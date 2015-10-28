package assignmenttwo.server

/**
 *
 */
object Permission {
  def defaultPermissions() : Seq[Permission] = Seq(Permission(999), Permission(998), Permission(978), Permission(932))
}

case class Permission(id : Int)
