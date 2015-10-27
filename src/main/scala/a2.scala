import java.util.concurrent.atomic.AtomicReference

import myactors._

import scala.concurrent.ExecutionContext



object a2 extends App {
  val actorOne = new TestActor("myactors.Actor One")
  val actorTwo = new TestActor("myactors.Actor Two")
  val actorThree = new TestActor("myactors.Actor Three")

  ConcurrentUtils.execute{
    actorOne.thisRef.!(ActorMessage("Hello!", actorTwo.thisRef))
  }
  ConcurrentUtils.execute{
    actorThree.thisRef.!(ActorMessage("Hello!", actorTwo.thisRef))
  }
  Thread.sleep(10000)
//  actorOne.onReceive(myactors.ActorMessage("Hello", myactors.NoRef(myactors.NoActor())))

}

