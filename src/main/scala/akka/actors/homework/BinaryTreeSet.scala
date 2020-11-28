package akka.actors.homework

import akka.actor._

object BinaryTreeSet {

  sealed trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  // requests with identifier `id`
  // `requester` should be notified when an operation is completed.
  object Operation {
    // insert an element `elem` into the tree.
    case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

    // check whether an element `elem` is present in the tree
    case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

    // remove the element `elem` from the tree
    case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation
  }

  sealed trait OperationReply {
    def id: Int
  }

  object OperationReply {
    // answer to the Contains request with identifier `id`.
    // `result` is true if and only if the element is present in the tree
    case class ContainsResult(id: Int, result: Boolean) extends OperationReply

    // successful completion of an insert or remove operation
    case class OperationFinished(id: Int) extends OperationReply
  }
}

class BinaryTreeSet extends Actor {
  import BinaryTreeSet._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  def receive: Receive = {
    case m: Operation => root ! m
  }
}
