package client
import cross.HeatConstants
import org.freedesktop.cairo.Context
/**
  * Painter class that paints on a context.
  * @param numberOfRows
  *                     The number of rows.
  * @param numberOfColumns
  *                        The number of columns.
  * @param context
  *                The context to paint on.
  */
class Painter(numberOfRows : Int, numberOfColumns : Int, context : Context) {
  def draw(reds : Array[Double]) : Unit = {
    println(reds.size)
    val redIt = reds.iterator
      for (row <- 0 until numberOfRows) {
        for (column <- 0 until numberOfColumns) {
          val next : Double = redIt.next()
          var red : Double = 0
          var blue : Double = 0 
          var green : Double = 0
          val temp = next
          if(temp <  10000 / 8){
            red = (temp * 255 * 8 / 10000)
            green = 0
            blue = 0
          }else if(temp  <  1000 / 4){
            red = 255
            green = (temp * 255 * 4 / 10000)
            blue = 0
          }else if(temp  < 1000){
            red = 255
            green = 255
            blue = (temp * 255 / 10000)
          }else{
            red=(255 - (temp * 255 / Double.MaxValue))
            green=(255 - (temp * 255 / Double.MaxValue))
            blue=255
          }
          context.setSource(red, green , blue, 1.0)
          context.rectangle(column * 2, row * 2, 2, 2)
          context.fill()
        }
      }
    }
}
