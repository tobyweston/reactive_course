package kvstore

import akka.actor._
import scala.concurrent.duration._
import scala.Some
import kvstore.Replicator.{Replicated, Replicate}

object ReplicationTimer {
  def props(replica: ActorRef): Props = Props(new ReplicationTimer(replica))
}

class ReplicationTimer(val replica: ActorRef) extends Actor {
  import context.dispatcher

  var replicator = context.system.actorOf(Replicator.props(replica))
  var waiting = Map.empty[Long, ActorRef]
  var timeouts = Map.empty[Long, Cancellable]


  override def postStop() = {
    timeouts.foreach {
      case (_, timeout) => timeout.cancel()
    }
    waiting = Map.empty
    context.stop(replicator)
  }

  def clearTimeout(id: Long) = {
    timeouts.get(id) match {
      case Some(cancellable) => {
        cancellable.cancel()
        timeouts - id
      }
      case None =>
    }
  }

  def clear(id: Long) = {
    timeouts -= id
    waiting -= id
  }

  def receive = {
    case msg: Replicate => {
      replicator ! msg
      val timeout = context.system.scheduler.scheduleOnce(1 second) { clear(msg.id) }
      timeouts = timeouts.updated(msg.id, timeout)
      waiting = waiting.updated(msg.id, sender)
    }
    case msg: Replicated => {
      clearTimeout(msg.id)
      waiting.get(msg.id) match {
        case Some(actor) => {
          actor ! msg
          waiting -= msg.id
        }
        case None =>
      }
    }
  }
}
