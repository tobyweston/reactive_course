package kvstore

import akka.actor._
import kvstore.Arbiter._
import scala.Some
import kvstore.CoOrdinator.{Timeout, Done, ReplicateAndPersist}
import kvstore.Persistence.{Persisted, Persist}

object Replica {
  sealed trait Operation {
    def key: String
    def id: Long
  }
  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply
  case class OperationAck(id: Long) extends OperationReply
  case class OperationFailed(id: Long) extends OperationReply
  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor {
  import Replica._
  import Replicator._

  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  var coOrdinator = context.system.actorOf(CoOrdinator.props(persistenceProps))

  var kv = Map.empty[String, String]

  var waiting = Map.empty[Long, ActorRef]

  def add(key: String, value: String) = { kv = kv.updated(key, value) }

  def remove(key: String) = { kv = kv - key }

  def update(key: String, valueOption: Option[String]) = valueOption match {
    case Some(value) => add(key, value)
    case None => remove(key)
  }

  def receive = {
    case JoinedPrimary   => context.become(leader)
    case JoinedSecondary => context.become(replica)
  }

  /* TODO Behavior for  the leader role. */
  val leader: Receive = {
    case msg: Replicas => {
      coOrdinator ! Replicas(msg.replicas - context.self)
      kv.foreach {
        case (key, value) => coOrdinator ! Replicate(key, Some(value), 0)
      }
    }
    case msg: Insert => {
      add(msg.key, msg.value)
      coOrdinator ! ReplicateAndPersist(msg.key, Some(msg.value), msg.id)
      waiting = waiting.updated(msg.id, sender)
    }
    case msg: Remove => {
      remove(msg.key)
      coOrdinator ! ReplicateAndPersist(msg.key, None, msg.id)
      waiting = waiting.updated(msg.id, sender)
    }
    case Get(key, id) => {
      sender ! GetResult(key, kv.get(key), id)
    }
    case Done(id) => {
      waiting.get(id) match {
        case Some(actor) => {
          actor ! OperationAck(id)
          waiting -= id
        }
        case None =>
      }
    }
    case Timeout(id) => {
      waiting.get(id) match {
        case Some(actor) => {
          actor ! OperationFailed(id)
          waiting -= id
        }
        case None =>
      }
    }
    case OperationAck(id) =>
    case _ =>
  }

  var expected: Long = 0

  /* TODO Behavior for the replica role. */
  val replica: Receive = {
    case Get(key, id) => {
      sender ! GetResult(key, kv.get(key), id)
    }
    case msg: Snapshot => {
      if(msg.seq == expected) {
        update(msg.key, msg.valueOption)
        coOrdinator ! Persist(msg.key, msg.valueOption, msg.seq)
        waiting = waiting.updated(msg.seq, sender)
      }

      if(msg.seq < expected) {
        sender ! SnapshotAck(msg.key, msg.seq)
        expected = math.max(expected, msg.seq + 1)
      }
    }
    case msg: Persisted => {
      waiting.get(msg.id) match {
        case Some(actor) => {
          actor ! SnapshotAck(msg.key, msg.id)
          expected = math.max(expected, msg.id + 1)
          waiting -= msg.id
        }
        case None =>
      }
    }
    case _ =>
  }

  arbiter ! Join
}
