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

  def insertRightOrLeft(m: Insert): Unit =
      // Compare this node value with new value
      if (m.elem == elem) {
        // Node with such value exists.
        // Inform the requester
        m.requester ! OperationFinished(m.id)
      } else {
        val direction = if (m.elem > elem) Right else Left
        // If the according sub-node exists, check there
        // Otherwise create new sub-node and let it inform the requester
        subtrees.get(direction) match {
          case Some(node) => node ! m
          case None =>
            val newActor = context.actorOf(BinaryTreeNode.props(m.elem, false))
            subtrees += direction -> newActor
            newActor ! m
        }
      }


  def isExistedElement(m: Contains): Unit =
    // Compare this node value with checked value
    if (m.elem == elem) {
      // Node with such value exists.
      // Inform the requester
      m.requester ! ContainsResult(m.id, !removed)
    } else {
      val direction = if (m.elem > elem) Right else Left
      // If the according sub-node exists, check there
      // Otherwise inform that node does not exist
      subtrees.get(direction) match {
        case Some(node) => node ! m
        case None => m.requester ! ContainsResult(m.id, false)
      }
    }


  def removeElement(m: Remove): Unit =
    // Compare this node value with remove value
    if (m.elem == elem) {
      // Node with such value exists.
      // Mark the flag and inform the requester
      removed = true
      m.requester ! OperationFinished(m.id)
    } else {
      val direction = if (m.elem > elem) Right else Left
      // If the according sub-node exists, check there
      // Otherwise inform that operation is completed
      subtrees.get(direction) match {
        case Some(node) => node ! m
        case None => m.requester ! OperationFinished(m.id)
      }
    }

  def receive: Receive = {
    case m: Insert    => insertRightOrLeft(m)
    case m: Contains  => isExistedElement(m)
    case m: Remove    => removeElement(m)
  }
}
