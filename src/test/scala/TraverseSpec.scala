
import org.scalatest.{Matchers, WordSpec}

import scala.collection.immutable._
import scala.collection.mutable

class TraverseSpec extends WordSpec with Matchers {
  "depth first" in {
    val exp = Add(Con(4), Sub(Con(10), Con(5)))
    val queue = depthFirst(exp)
    println(queue)
  }

  "midLeftRight" in {
    val exp = Add(Con(4), Sub(Con(10), Con(5)))
    val queue = midLeftRight(exp)
    println(queue)
    println(queue.dequeue)
    println()
  }

  // stack deprecated. wtf.
  "midLeftRightStack" in {
    val exp = Add(Con(4), Sub(Con(10), Con(5)))
    val stack = midLeftRightStack(exp)
    println(exp)
    println(stack)
    println((stack.head, stack.pop))
    println((stack.pop.head, stack.pop.pop))
    println()
  }

  "midLeftRightList" in {
    val exp = Add(Mul(Con(4), Con(2)), Sub(Con(10), Con(5)))
    val list = midLeftRightList(exp)
    println(exp)
    println(list)
    println((list.head, list.tail))
    println((list.tail.head, list.tail.tail))
    println()
  }

  def depthFirst(exp: Exp): Queue[String] = {
    def loop(exp: Exp, queue: Queue[String]): Queue[String] = {
      exp match {
        case Con(value) => queue.enqueue(value.toString)
        case Var(symbol) => queue.enqueue(symbol.toString)
        case Add(lhs, rhs) => loop(lhs, queue).enqueue(loop(rhs, queue)).enqueue("+")
        case Sub(lhs, rhs) => loop(lhs, queue).enqueue(loop(rhs, queue)).enqueue("-")
      }
    }
    loop(exp, Queue.empty[String])
  }

  def midLeftRight(exp: Exp): Queue[String] = {
    def loop(exp: Exp, queue: Queue[String]): Queue[String] = {
      exp match {
        case Con(value) => queue.enqueue(value.toString)
        case Var(symbol) => queue.enqueue(symbol.toString)
        case Add(lhs, rhs) => queue.enqueue("+").enqueue(loop(lhs, queue)).enqueue(loop(rhs, queue))
        case Sub(lhs, rhs) => queue.enqueue("-").enqueue(loop(lhs, queue)).enqueue(loop(rhs, queue))
      }
    }
    loop(exp, Queue.empty[String])
  }

  def midLeftRightList(exp: Exp): List[String] = {
    def loop(exp: Exp, list: List[String]): List[String] = {
      exp match {
        case Con(value) => value.toString :: list
        case Var(symbol) => symbol.toString :: list
        case Add(lhs, rhs) => "+" :: loop(lhs, list) ::: loop(rhs, list)
        case Sub(lhs, rhs) => "-" :: loop(lhs, list) ::: loop(rhs, list)
        case Mul(lhs, rhs) => "*" :: loop(lhs, list) ::: loop(rhs, list)
        case Div(lhs, rhs) => "/" :: loop(lhs, list) ::: loop(rhs, list)
      }
    }
    loop(exp, List.empty[String]).reverse
  }

  def midLeftRightStack(exp: Exp): Stack[String] = {
    def loop(exp: Exp, stack: Stack[String]): Stack[String] = {
      exp match {
        case Con(value) => stack.push(value.toString)
        case Var(symbol) => stack.push(symbol.toString)
        case Add(lhs, rhs) => stack.push("+").pushAll(loop(lhs, stack)).pushAll(loop(rhs, stack))
        case Sub(lhs, rhs) => stack.push("-").pushAll(loop(lhs, stack)).pushAll(loop(rhs, stack))
        case Mul(lhs, rhs) => stack.push("*").pushAll(loop(lhs, stack)).pushAll(loop(rhs, stack))
        case Div(lhs, rhs) => stack.push("/").pushAll(loop(lhs, stack)).pushAll(loop(rhs, stack))
      }
    }
    loop(exp, Stack.empty[String])
  }
}
