package kvstore

import akka.actor._
import scala.concurrent.duration._
import scala.Some
import kvstore.Replicator.{Replicated, Replicate}
import kvstore.Arbiter.Replicas

object ReplicationCollector {
  def props(): Props = Props(new ReplicationCollector())
}

class ReplicationCollector extends Actor {
  import context.dispatcher

  var replicators = Set.empty[ActorRef]
  var secondaries = Map.empty[ActorRef, ActorRef]

  var waiting = Map.empty[Long, (ActorRef, String)]
  var respondents = Map.empty[Long, Set[ActorRef]]
  var timeouts = Map.empty[Long, Cancellable]

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
    respondents -= id
  }

  def synchronise(replicas: Set[ActorRef]) = {
    replicas.filterNot(replicators.contains).foreach(replica => {
      val replicator = context.system.actorOf(ReplicationTimer.props(replica))
      replicators = replicators + replicator
      secondaries = secondaries.updated(replica, replicator)
    })

    secondaries.keys.filterNot(replicas.contains).foreach(replica => {
      val replicator = secondaries.get(replica).get
      context.stop(replicator)
      replicators -= replicator
      secondaries -= replica
    })

    if(replicators.isEmpty) {
      timeouts.foreach {
        case (_, timeout) => timeout.cancel()
      }
      waiting.foreach {
        case (id, (actor, key)) => actor ! Replicated(key, id)
      }
    }
  }
  
  def receive = {
    case Replicas(replicas) => synchronise(replicas)
    case msg: Replicate => {
      if(replicators.isEmpty) sender ! Replicated(msg.key, msg.id)
      else {
        replicators.foreach {
          case replicator => replicator ! msg
        }
        val timeout = context.system.scheduler.scheduleOnce(1 second) { clear(msg.id) }
        timeouts = timeouts.updated(msg.id, timeout)
        respondents = respondents.updated(msg.id, replicators)
        waiting = waiting.updated(msg.id, (sender, msg.key))
      }
    }
    case msg: Replicated => {
      respondents = respondents.updated(msg.id, respondents.getOrElse(msg.id, Set()) - sender)
      if(respondents.get(msg.id).get.filter(replicators.contains).isEmpty) {
        clearTimeout(msg.id)
        waiting.get(msg.id) match {
          case Some((actor, key)) => {
            actor ! msg
            waiting -= msg.id
            respondents -= msg.id
          }
          case None =>
        }
      }
    }
  }
}
