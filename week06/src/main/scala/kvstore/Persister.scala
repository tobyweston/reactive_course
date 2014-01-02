package kvstore

import akka.actor._
import akka.actor.SupervisorStrategy.Restart
import scala.concurrent.duration._
import scala.Some

object Persister {

  def props(persistenceProps: Props): Props = Props(new Persister(persistenceProps))
}

class Persister(persistenceProps: Props) extends Actor {
  import Persistence._
  import context.dispatcher

  override def supervisorStrategy = OneForOneStrategy() {
    case e: PersistenceException => Restart
  }

  var persistence = context.system.actorOf(persistenceProps)

  var waiting = Map.empty[Long, ActorRef]
  var retries = Map.empty[Long, Cancellable]
  var timeouts = Map.empty[Long, Cancellable]

  def clear(map: Map[Long, Cancellable], id: Long): Map[Long, Cancellable] = {
    map.get(id) match {
      case Some(cancellable) => {
        cancellable.cancel()
        map - id
      }
      case None => map
    }
  }

  def clearRetry(id: Long) = { retries = clear(retries, id) }
  def clearTimeout(id: Long) = { timeouts = clear(timeouts, id) }

  def clearAll(id: Long) = {
    clearRetry(id)
    clearTimeout(id)
    waiting -= id
  }

  def respondOk(msg: Persisted) = {
    clearTimeout(msg.id)
    clearRetry(msg.id)
    waiting.get(msg.id) match {
      case Some(actor) => {
        waiting -= msg.id
        actor ! msg
      }
      case None =>
    }
  }

  def receive = {
    case msg: Persist => {
      val retry = context.system.scheduler.schedule(0 millis, 100 millis, persistence, msg)
      val timeout = context.system.scheduler.scheduleOnce(1 second) { clearAll(msg.id) }
      retries = retries.updated(msg.id, retry)
      timeouts = timeouts.updated(msg.id, timeout)
      waiting = waiting.updated(msg.id, sender)
    }
    case msg: Persisted => respondOk(msg)
  }
}
