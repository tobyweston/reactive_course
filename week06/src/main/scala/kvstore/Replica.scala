package kvstore

import akka.actor._
import kvstore.Arbiter._
import scala.collection.immutable.Queue
import akka.actor.SupervisorStrategy.Restart
import scala.annotation.tailrec
import akka.pattern.{ ask, pipe }
import scala.concurrent.duration._
import akka.util.Timeout
import scala.Some

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
  import Persistence._
  import context.dispatcher

  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  var persistence = context.system.actorOf(persistenceProps)

  var kv = Map.empty[String, String]
  // a map from secondary replicas to replicators
  var secondaries = Map.empty[ActorRef, ActorRef]
  // the current set of replicators
  var replicators = Set.empty[ActorRef]

  var unpersistedOperations = Map.empty[Long, (ActorRef, Operation, Cancellable)]
  var unpersistedSnapshots = Map.empty[Long, (ActorRef, Snapshot, Cancellable)]


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
      persistInsert(sender, msg)
    }
    case msg: Remove => {
      remove(msg.key)
      persistRemove(sender, msg)
    }
    case Get(key, id) => {
      sender ! GetResult(key, kv.get(key), id)
    }
    case Persisted(key, id) => {
      unpersistedOperations.get(id).foreach(acknowledgeOperation)
      unpersistedOperations = unpersistedOperations - id
    }
    case _ =>
  }

  def persistInsert(sender: ActorRef, msg: Insert) = {
    val cancellable = context.system.scheduler.schedule(0 millis, 100 millis, persistence, Persist(msg.key, Some(msg.value), msg.id))
    unpersistedOperations = unpersistedOperations.updated(msg.id, (sender, msg, cancellable));
  }

  def persistRemove(sender: ActorRef, msg: Remove) = {
    val cancellable = context.system.scheduler.schedule(0 millis, 100 millis, persistence, Persist(msg.key, None, msg.id))
    unpersistedOperations = unpersistedOperations.updated(msg.id, (sender, msg, cancellable));
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
        val cancellable = context.system.scheduler.schedule(0 millis, 100 millis, persistence, Persist(msg.key, msg.valueOption, msg.seq))
        unpersistedSnapshots = unpersistedSnapshots.updated(msg.seq, (sender, msg, cancellable))
      }

      if(msg.seq < expected) {
        sender ! SnapshotAck(msg.key, msg.seq)
        expected = math.max(expected, msg.seq + 1)
      }
    }
    case Persisted(key, seq) => {
      unpersistedSnapshots.get(seq).foreach(acknowledgeSnapshot)
      unpersistedSnapshots = unpersistedSnapshots - seq
      expected = math.max(expected, seq + 1)
    }
    case _ =>
  }

  def acknowledgeOperation(tuple: (ActorRef, Operation, Cancellable)) = tuple match {
    case (actor, msg, cancellable) => {
      actor ! OperationAck(msg.id)
      cancellable.cancel()
    }
  }

  def acknowledgeSnapshot(tuple: (ActorRef, Snapshot, Cancellable)) = tuple match {
    case (actor, msg, cancellable) => {
      actor ! SnapshotAck(msg.key, msg.seq)
      cancellable.cancel()
    }
  }

  arbiter ! Join
}
