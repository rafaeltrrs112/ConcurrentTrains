package assignmenttwo
import server._
import myactors._




object a2 extends App {
  val actorOne = new TestActor("myactors.Actor One")
  val actorTwo = new TestActor("myactors.Actor Two")
  val actorThree = new TestActor("myactors.Actor Three")

  utils.execute{
    actorOne.thisRef.!(ActorMessage("Hello!", actorTwo.thisRef))
  }
  utils.execute{
    actorThree.thisRef.!(ActorMessage("Hello!", actorTwo.thisRef))
  }
  Thread.sleep(10000)

}

