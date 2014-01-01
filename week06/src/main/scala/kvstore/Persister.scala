package kvstore

import akka.actor._
import akka.actor.SupervisorStrategy.Restart
import scala.concurrent.duration._
import scala.Some
import kvstore.Persister.{PersistAck, PersistOp}

object Persister {

  case class PersistOp(dest: ActorRef, key: String, valueOption: Option[String], id: Long)
  case class PersistAck(dest: ActorRef, key: String, id: Long)

  def props(persistenceProps: Props): Props = Props(new Persister(persistenceProps))
}

class Persister(persistenceProps: Props) extends Actor {
  import Persistence._
  import context.dispatcher

  override def supervisorStrategy = OneForOneStrategy() {
    case _: PersistenceException => Restart
  }

  var persistence = context.system.actorOf(persistenceProps)

  var persisting = Map.empty[Long, (ActorRef, PersistOp, Cancellable)]

  def receive = {
    case msg: PersistOp => {
      val retry = context.system.scheduler.schedule(0 millis, 100 millis, persistence, Persist(msg.key, msg.valueOption, msg.id))
      persisting = persisting.updated(msg.id, (sender, msg, retry))
    }
    case msg: Persisted => {
      persisting.get(msg.id) match {
        case Some((replica, msg, retry)) => {
          retry.cancel()
          replica ! PersistAck(msg.dest, msg.key, msg.id)
          persisting -= msg.id
        }
      }
    }
  }

}
