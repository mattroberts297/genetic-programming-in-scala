
import scala.collection.immutable._
import scala.collection.mutable
import org.slf4s.Logging

import scala.util.Try

object Main extends App with Logging {
  log.debug("Main started")
  val terminalSet = Constants(-5f, 5f, 0.5f)
  val functionSet = Set(Add, Sub, Div, Mul)

}

object Constants {
  def apply(from: Float, to: Float, step: Float): Set[Constant] = {
    from.to(to, step).map(Constant).toSet
  }
}

trait Terminal {
  def apply: Float
}

case class Variable(name: String, value: Float) extends Terminal {
  def apply: Float = value
}

case class Constant(c: Float) extends Terminal {
  def apply: Float = c
}

trait BinaryFunction {
  def apply(lhs: Float, rhs: Float): Float
}

case object Add extends BinaryFunction {
  def apply(lhs: Float, rhs: Float): Float = lhs + rhs
}

case object Sub extends BinaryFunction {
  def apply(lhs: Float, rhs: Float): Float = lhs - rhs
}

case object Div extends BinaryFunction {
  def apply(lhs: Float, rhs: Float): Float = Try(lhs / rhs).getOrElse(1f)
}

case object Mul extends BinaryFunction {
  def apply(lhs: Float, rhs: Float): Float = lhs * rhs
}
