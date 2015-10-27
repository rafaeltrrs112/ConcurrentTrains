package myactors

/**
 * Assignment 2 : Implement a Concurrent Data Structure and use it in some Application.
 */
class Node[E](var value : Option[E] = None, @volatile var next : Node[E]){
  override def toString : String = value.toString
}
