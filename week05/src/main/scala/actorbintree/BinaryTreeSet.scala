/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import scala.collection.immutable.Queue

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply
  
  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}


class BinaryTreeSet extends Actor with ActorLogging {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  // optional
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    case op: Operation => root ! op
    case GC => {
      val newRoot = createRoot
      root ! CopyTo(newRoot)
      context.become(garbageCollecting(newRoot))
    }
  }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case op: Operation => pendingQueue = pendingQueue.enqueue(op)
    case GC => //ignore
    case CopyFinished =>
      pendingQueue = drain(pendingQueue) { newRoot ! _ }
      context.stop(root)
      root = newRoot
  }

  def drain[T](queue: Queue[T])(f: T => Unit): Queue[T] = queue match {
    case q if q.isEmpty => q
    case q =>
      val (item, rest) = q.dequeue
      f(item)
      drain(rest)(f)
  }
}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor with ActorLogging {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  // optional
  def receive = normal

  def insert(position: Position)(requester: ActorRef, id: Int, value: Int) = {
    if(subtrees.contains(position)) subtrees(position) ! Insert(requester, id, value)
    else {
      subtrees = subtrees.updated(position, context.actorOf(BinaryTreeNode.props(value, initiallyRemoved = false)))
      requester ! OperationFinished(id)
    }
  }

  def remove(position: Position)(requester: ActorRef, id: Int, value: Int) = {
    if(subtrees.contains(position)) subtrees(position) ! Remove(requester, id, value)
    else requester ! OperationFinished(id)
  }

  def contains(position: Position)(requester: ActorRef, id: Int, value: Int) = {
    if(subtrees.contains(position)) subtrees(position) ! Contains(requester, id, value)
    else requester ! ContainsResult(id, result = false)
  }

  def copy(position: Position)(node: ActorRef): Option[ActorRef] = {
    if(subtrees.contains(position)) {
      subtrees(position) ! CopyTo(node)
      Some(subtrees(position))
    } else {
      None
    }
  }

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {
    case Insert(requester, id, value) => {
      if(value < elem) insert(Left)(requester, id, value)
      else if(value > elem) insert(Right)(requester, id, value)
      else {
        removed = false
        requester ! OperationFinished(id)
      }
    }
    case Remove(requester, id, value) => {
      if(value < elem) remove(Left)(requester, id, value)
      else if(value > elem) remove(Right)(requester, id, value)
      else {
        removed = true
        requester ! OperationFinished(id)
      }
    }
    case Contains(requester, id, value) => {
      if(value < elem) contains(Left)(requester, id, value)
      else if(value > elem) contains(Right)(requester, id, value)
      else requester ! ContainsResult(id, !removed)
    }
    case CopyTo(node) => {
      if(!removed) node ! Insert(context.self, elem, elem)
      context.become(copying((Set() + copy(Left)(node) + copy(Right)(node)).flatten, removed))
    }
  }

  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = {
    if(expected.isEmpty && insertConfirmed) {
      context.parent ! CopyFinished
      normal
    } else {
      case OperationFinished => context.become(copying(expected, true))
      case CopyFinished => context.become(copying(expected - sender, insertConfirmed))
    }
  }
}
