package kvstore

import akka.actor.{Props, Actor}
import scala.util.Random

object Persistence {
  case class Persist(key: String, valueOption: Option[String], id: Long)
  case class Persisted(key: String, id: Long)

  class PersistenceException(persist: Persist) extends Exception(String valueOf persist) {
    setStackTrace(Array.empty[StackTraceElement])
  }

  def props(flaky: Boolean): Props = Props(classOf[Persistence], flaky)
}

class Persistence(flaky: Boolean) extends Actor {
  import Persistence._

  def receive = {
    case msg: Persist =>
      if (!flaky || Random.nextBoolean()) sender ! Persisted(msg.key, msg.id)
      else throw new PersistenceException(msg)
  }

}
