import scala.collection.immutable._
import org.slf4s.Logging
import model._

object Main extends App with Logging {
  import GP._
  val maxDepth = 6
  val terminalSet = IndexedSeq(Var('x)) ++ 1f.to(5f, 1f).map(Con)
  val functionSet = IndexedSeq(Add, Sub, Div, Mul)
  def pow(a: Float, b: Float): Float = Math.pow(a, b).toFloat
//  val cases = (-3f).to(3f, 0.5f).map(x => (Map('x -> x), pow(x, 2) - x - 2)).toMap
  val cases = (-3f).to(3f, 0.5f).map(x => (Map('x -> x), pow(x,3) / 4 + 3 * pow(x, 2) / 4 - 3 * x / 2 - 2)).toMap
  def criteria(fitness: Float): Boolean = fitness < 0.01f
  val start = System.currentTimeMillis()
  val fitTree = run(functionSet, terminalSet, cases, fitness, criteria, populationSize = 1000)
  val finish = System.currentTimeMillis()
  val timeToRunInMilliseconds = (finish - start)
  val timeToRunInSeconds = (finish - start) / 1000
  log.debug(s"Fittest tree: ${fitTree}")
  log.debug("expected\t\tactual")
  cases.foreach { case (symbols, expected) =>
    log.debug(s"${expected}\t${Exp.eval(fitTree, symbols)}")
  }
  log.debug(s"timeToRunInMilliseconds=$timeToRunInMilliseconds")
  log.debug(s"timeToRunInSeconds=$timeToRunInSeconds")
}
