package kvstore

import akka.actor._
import scala.concurrent.duration._
import scala.Some
import kvstore.Replicator.{Replicated, Replicate}
import kvstore.CoOrdinator.{Timeout, Done, ReplicateAndPersist}
import kvstore.Persistence.{Persisted, Persist}
import kvstore.Arbiter.Replicas

object CoOrdinator {

  case class ReplicateAndPersist(key: String, valueOption: Option[String], id: Long)
  case class Done(id: Long)
  case class Timeout(id: Long)

  def props(persistenceProps: Props): Props = Props(new CoOrdinator(persistenceProps: Props))
}

class CoOrdinator(persistenceProps: Props) extends Actor {
  import context.dispatcher

  val persister = context.system.actorOf(Persister.props(persistenceProps))
  val replicator = context.system.actorOf(ReplicationCollector.props())

  var waiting = Map.empty[Long, ActorRef]
  var replicating = Map.empty[Long, Boolean]
  var persisting = Map.empty[Long, Boolean]
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

  def respondTimeout(id: Long) = {
    timeouts -= id
    persisting -= id
    replicating -= id
    waiting.get(id) match {
      case Some(actor) => {
        actor ! Timeout(id)
        waiting -= id
      }
      case None =>
    }
  }

  def justPersisting(id: Long) = !replicating.contains(id) && persisting.getOrElse(id, false)
  def allDone(id: Long) = replicating.getOrElse(id, true) && persisting.getOrElse(id, true)

  def respondOk(id: Long) = {
    clearTimeout(id)
    persisting -= id
    replicating -= id
    waiting.get(id) match {
      case Some(actor) => {
        actor ! Done(id)
        waiting -= id
      }
      case None =>
    }
  }

  def respondPersisted(msg: Persisted) = {
    clearTimeout(msg.id)
    persisting -= msg.id
    waiting.get(msg.id) match {
      case Some(actor) => {
        actor ! msg
        waiting -= msg.id
      }
      case None =>
    }
  }

  def receive = {
    case msg: Replicas => replicator ! msg
    case msg: Replicate => {
      replicator ! msg
      waiting = waiting.updated(msg.id, sender)
      replicating = replicating.updated(msg.id, false)
    }
    case msg: Persist => {
      persister ! msg
      val timeout = context.system.scheduler.scheduleOnce(1 second) { respondTimeout(msg.id) }
      timeouts = timeouts.updated(msg.id, timeout)
      waiting = waiting.updated(msg.id, sender)
      persisting = persisting.updated(msg.id, false)
    }
    case msg: ReplicateAndPersist => {
      persister ! Persist(msg.key, msg.valueOption, msg.id)
      replicator ! Replicate(msg.key, msg.valueOption, msg.id)
      val timeout = context.system.scheduler.scheduleOnce(1 second) { respondTimeout(msg.id) }
      timeouts = timeouts.updated(msg.id, timeout)
      waiting = waiting.updated(msg.id, sender)
      persisting = persisting.updated(msg.id, false)
      replicating = replicating.updated(msg.id, false)
    }
    case msg: Persisted => {
      persisting = persisting.updated(msg.id, true)
      if(justPersisting(msg.id)) respondPersisted(msg)
      if(allDone(msg.id)) respondOk(msg.id)
    }
    case msg: Replicated => {
      replicating = replicating.updated(msg.id, true)
      if(allDone(msg.id)) respondOk(msg.id)
    }
    case _ =>
  }
}
