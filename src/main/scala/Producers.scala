import scala.actors.Actor
import scala.actors.Actor._

class PreOrder(n: Tree) extends Producer[Int] {
  def produceValues = traverse(n)
  def traverse(n: Tree) {
    if (n != null) {
      produce(n.elem)
      traverse(n.left)
      traverse(n.right)
    }
  }
}


abstract class Producer[T] {

  protected def produceValues: Unit

  protected def produce(x: T) {
    coordinator ! Some(x)
    receive { case Next => }
  }

  case object Next
  case object Stop

  private val Undefined = new Object

  private val producer: Actor = actor {
    receive {
      case Next =>
        produceValues
        coordinator ! None
    }
  }

  private val coordinator: Actor = actor {
    loop {
      react {
        case Next =>
          producer ! Next
          reply {
            receive { case x: Option[_] => x }
          }
        case Stop => exit('stop)
      }
    }
  }


  def iterator = new Iterator[T] {
    private var current: Any = Undefined

    private def lookAhead = {
      if (current == Undefined) current = coordinator !? Next
      current
    }

    def hasNext: Boolean = lookAhead match {
      case Some(x) => true
      case None => { coordinator ! Stop; false }
    }

    def next: T = lookAhead match {
      case Some(x) =>
        current = Undefined
        x.asInstanceOf[T]
      case None =>
        throw new NoSuchElementException("next on empty iterator")
    }
  }
}

class Tree(val elem: Int, val left: Tree = null, val right: Tree = null)

object ProducersExample {
  def main(args: Array[String]) {
    // Create a binary tree:
    //        1
    //       / \
    //      2   3
    //     / \ / \
    //    4  5 6  7
    val tree = new Tree(1,
      new Tree(2,
        new Tree(4),
        new Tree(5)),
      new Tree(3,
        new Tree(6),
        new Tree(7)))

    println("Pre-order traversal:")
    val preOrder = new PreOrder(tree)
    val preOrderIter = preOrder.iterator
    while (preOrderIter.hasNext) {
      print(preOrderIter.next + " ")
    }
    println()

    println("\nUsing foreach with pre-order:")
    preOrder.iterator.foreach(x => print(x + " "))
    println()

    println("\nCollect pre-order values to a list and transform:")
    val list = preOrder.iterator.toList
    println("Original: " + list)
    println("Doubled:  " + list.map(_ * 2))

    println("\nProducer example completed successfully!")
  }
}