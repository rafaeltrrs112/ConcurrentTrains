import cross.Cell

/**
  * Created by rtorres12 on 12/10/15.
  */
object Katana {
  def slice(alloy : List[List[Cell]]) : ((List[List[Cell]], List[Cell]), (List[List[Cell]], List[Cell])) = {
    val sliced : List[List[List[Cell]]] = alloy.grouped(2).toList

    val top = sliced.head
    val bottom = sliced.last

    val topDepend = bottom.head
    val bottomDepend = top.last
    ((top, topDepend), (bottom, bottomDepend))
  }
}
