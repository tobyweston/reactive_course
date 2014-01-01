package kvstore

import akka.actor._
import kvstore.Arbiter._
import scala.Some
import kvstore.Persister.{PersistAck, PersistOp}

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

  var persister = context.system.actorOf(Persister.props(persistenceProps))

  var kv = Map.empty[String, String]
  // a map from secondary replicas to replicators
  var secondaries = Map.empty[ActorRef, ActorRef]
  // the current set of replicators
  var replicators = Set.empty[ActorRef]

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
    case msg: Insert => {
      add(msg.key, msg.value)
      sender ! OperationAck(msg.id)
    }
    case msg: Remove => {
      remove(msg.key)
      sender ! OperationAck(msg.id)
    }
    case Get(key, id) => {
      sender ! GetResult(key, kv.get(key), id)
    }
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
        persister ! PersistOp(sender, msg.key, msg.valueOption, msg.seq)
      }

      if(msg.seq < expected) {
        sender ! SnapshotAck(msg.key, msg.seq)
        expected = math.max(expected, msg.seq + 1)
      }
    }
    case msg: PersistAck => {
      msg.dest ! SnapshotAck(msg.key, msg.id)
      expected = math.max(expected, msg.id + 1)
    }
    case _ =>
  }

  arbiter ! Join
}
