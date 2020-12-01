package akka.actors.homework

import akka.actor.{Actor, ActorRef, Props}

object BinaryTreeNode {
  sealed trait Position

  case object Left extends Position
  case object Right extends Position

  def props(elem: Int, initiallyRemoved: Boolean): Props = Props(classOf[BinaryTreeNode], elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet.Operation._
  import BinaryTreeSet.OperationReply._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  def insertRightOrLeft(m: Insert) = m match {
    case insertMsg @ Insert(requester, id, newElem) =>
      // Compare this node value with new value
      if (newElem == elem) {
        // Node with such value exists.
        // Inform the requester
        requester ! OperationFinished(id)
      } else if (newElem > elem) {
        // If the right node exists, check there
        // Otherwise create right node and let it inform the requester
        subtrees.get(Right) match {
          case Some(node) => node ! insertMsg
          case None =>
            val rightActor = context.actorOf(BinaryTreeNode.props(newElem, false))
            subtrees += Right -> rightActor
            rightActor ! insertMsg
        }
      } else if (newElem < elem) {
        // If the left node exists, check there
        // Otherwise create left node and let it inform the requester
        subtrees.get(Left) match {
          case Some(node) => node ! insertMsg
          case None =>
            val leftActor = context.actorOf(BinaryTreeNode.props(newElem, false))
            subtrees += Left -> leftActor
            leftActor ! insertMsg
        }
      }
  }

  def isExistedElement(m: Contains) = m match {
    case containsMsg @ Contains(requester, id, checkedElem) =>
      // Compare this node value with new value
      if (checkedElem == elem) {
        // Node with such value exists.
        // Inform the requester
        requester ! ContainsResult(id, true)
      } else if (checkedElem > elem) {
        // If the right node exists, check there
        // Otherwise inform that node does not exist
        subtrees.get(Right) match {
          case Some(node) => node ! containsMsg
          case None => requester ! ContainsResult(id, false)
        }
      } else if (checkedElem < elem) {
        // If the left node exists, check there
        // Otherwise inform that node does not exist
        subtrees.get(Left) match {
          case Some(node) => node ! containsMsg
          case None => requester ! ContainsResult(id, false)
        }
      }
  }

  def removeElement(m: Remove) = ???

  def receive: Receive = {
    case m: Insert    => insertRightOrLeft(m)
    case m: Contains  => isExistedElement(m)
    case m: Remove    => removeElement(m)
  }
}
