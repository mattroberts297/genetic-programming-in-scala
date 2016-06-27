
import scala.Predef
import scala.collection.immutable._
import scala.collection.mutable
import org.slf4s.Logging

import scala.util.{Random, Try}

object Main extends App with Logging {
  log.debug("Main started")
  val constants = Constants.range(-5f, 5f, 1f)
  val variables = IndexedSeq(Var('x))
  val functions = IndexedSeq(Add, Sub, Div, Mul)

  val expected = (-1f).to(1f, 0.05f).map(x => (Map('x -> x), x * x + x + 1))
  val population = 1000
  val terminalSet = constants ++ variables
  val functionSet = functions
  val firstTrees = Trees.rampHalfHalf(population, 4, functionSet, terminalSet).toVector

  def tournament(a: (Exp, Float), b: (Exp, Float)): Exp = {
    val (aExp, aFit) = a
    val (bExp, bFit) = b
    if (aFit < bFit) aExp else bExp
  }

  def random[T](elements: IndexedSeq[T]): T = {
    elements(Random.nextInt(elements.length))
  }

  def loop(run: Int, trees: IndexedSeq[Exp], expected: Seq[(Map[Symbol, Float], Float)]): Exp = {
    log.debug(s"Run $run")
    val treesAndFitness = trees.map(tree => tree -> Trees.fitness(tree, expected))
    val sortedTreesAndFitness = treesAndFitness.sortBy { case (_, fitness) => fitness }
    val sortedTrees = treesAndFitness.map { case (tree, _) => tree }
    val (topTree, topFitness) = sortedTreesAndFitness.head
    if (topFitness < 0.1f || run == 1000) {
      topTree
    } else {
      val crossoverTrees = 1.to(trees.length / 4 * 3).map { _ =>
        Trees.crossover(
          tournament(random(treesAndFitness), random(treesAndFitness)),
          tournament(random(treesAndFitness), random(treesAndFitness)),
          functions)
      }
      val replicateTrees = sortedTrees.take(trees.length / 4)
      loop(run + 1, crossoverTrees ++ replicateTrees, expected)
    }
  }

  val fitTree = loop(1, firstTrees, expected)
  log.debug(s"Fittest tree: ${fitTree}")
  log.debug("expected\t\tactual")
  expected.foreach { case (symbols, expected) =>
    log.debug(s"${expected}\t${fitTree.eval(symbols)}")
  }
}

object Trees {
  def full(depth: Int, functions: IndexedSeq[(Exp, Exp) => Exp], terminals: IndexedSeq[Exp]): Exp = {
    def loop(i: Int): Exp = {
      if (i == depth) {
        terminals(Random.nextInt(terminals.length))
      } else {
        functions(Random.nextInt(functions.length))(loop(i + 1), loop(i + 1))
      }
    }
    loop(0)
  }

  def grow(depth: Int, functions: IndexedSeq[(Exp, Exp) => Exp], terminals: IndexedSeq[Exp]): Exp = {
    def randomStop: Boolean = {
      rand() < terminals.length.toFloat / (terminals.length + functions.length)
    }
    def loop(i: Int): Exp = {
      if (i == depth || randomStop) {
        terminals(Random.nextInt(terminals.length))
      } else {
        functions(Random.nextInt(functions.length))(loop(i + 1), loop(i + 1))
      }
    }
    loop(0)
  }

  def rampHalfHalf(
      count: Int,
      maxDepth: Int,
      functions: IndexedSeq[(Exp, Exp) => Exp],
      terminals: IndexedSeq[Exp]): Set[Exp] = {
    def loop(acc: Set[Exp], i: Int, depth: Int): Set[Exp] = {
      if(i == count) {
        acc
      } else {
        val tree = if (i % 2 == 0) {
          full(depth, functions, terminals)
        } else {
          grow(depth, functions, terminals)
        }
        val nextDepth = if (depth == maxDepth) 1 else depth + 1
        if (acc.contains(tree)) {
          loop(acc, i, nextDepth)
        } else {
          loop(acc + tree, i + 1, nextDepth)
        }
      }
    }
    loop(Set.empty, 0, 1)
  }

  def rand(): Float = Random.nextFloat()

  def crossover(left: Exp, right: Exp, functions: IndexedSeq[(Exp, Exp) => Exp]): Exp = {
    val lefts = collect(left)
    val rights = collect(right)
    val lhs = lefts(Random.nextInt(lefts.length))
    val rhs = rights(Random.nextInt(rights.length))
    (lhs, rhs) match {
      case (Add(lhs, _), _) => Add(lhs, rhs)
      case (Sub(lhs, _), _) => Sub(lhs, rhs)
      case (Mul(lhs, _), _) => Mul(lhs, rhs)
      case (Div(lhs, _), _) => Div(lhs, rhs)
      case (_, Add(_, rhs)) => Add(lhs, rhs)
      case (_, Sub(_, rhs)) => Add(lhs, rhs)
      case (_, Mul(_, rhs)) => Add(lhs, rhs)
      case (_, Div(_, rhs)) => Add(lhs, rhs)
      // This last one is actually a mutation. Nature finds a way.
      case (lhs, rhs) => functions(Random.nextInt(functions.length))(lhs, rhs)
    }
  }

  private def collect(tree: Exp): IndexedSeq[Exp] = {
    val ops = collectOps(tree)
    if (rand() > 0.9 || ops.isEmpty) {
      collectTerminals(tree)
    } else {
      ops
    }
  }

  private def collectTerminals(tree: Exp): IndexedSeq[Exp] = {
    def collect(acc: IndexedSeq[Exp], subtree: Exp): IndexedSeq[Exp] = {
      subtree match {
        case v: Var => IndexedSeq(v) ++ acc
        case c: Con => IndexedSeq(c) ++ acc
        case o: BinOp => collect(acc, o.lhs) ++ collect(acc, o.rhs)
      }
    }
    collect(IndexedSeq.empty, tree)
  }

  private def collectOps(tree: Exp): IndexedSeq[Exp with BinOp] = {
    def collectOps(acc: IndexedSeq[Exp with BinOp], subtree: Exp): IndexedSeq[Exp with BinOp] = {
      subtree match {
        case v: Var => acc
        case c: Con => acc
        case o: BinOp =>
          IndexedSeq(o) ++ collectOps(acc, o.lhs) ++ collectOps(acc, o.rhs)
      }
    }
    collectOps(IndexedSeq.empty, tree)
  }

  def fitness(tree: Exp, expected: Seq[(Map[Symbol, Float], Float)]): Float = {
    expected.foldLeft(0f) { case (acc, (symbols, expected)) =>
      acc + Math.pow((expected - tree.eval(symbols)), 2).toFloat
    }
  }
}

object Constants {
  def range(from: Float, to: Float, step: Float): IndexedSeq[Con] = {
    IndexedSeq(from.to(to, step).map(Con): _*)
  }
}

sealed trait Exp {
  def eval(implicit symbols: Map[Symbol, Float]): Float
}

sealed trait BinOp {
  def lhs: Exp
  def rhs: Exp
}

case class Add(lhs: Exp, rhs: Exp) extends Exp with BinOp {
  override def eval(implicit symbols: Map[Symbol, Float]): Float = {
    lhs.eval + rhs.eval
  }
}

case class Sub(lhs: Exp, rhs: Exp) extends Exp with BinOp {
  override def eval(implicit symbols: Map[Symbol, Float]): Float = {
    lhs.eval - rhs.eval
  }
}

case class Mul(lhs: Exp, rhs: Exp) extends Exp with BinOp {
  override def eval(implicit symbols: Map[Symbol, Float]): Float = {
    lhs.eval * rhs.eval
  }
}

case class Div(lhs: Exp, rhs: Exp) extends Exp with BinOp {
  override def eval(implicit symbols: Map[Symbol, Float]): Float = Try {
    lhs.eval / rhs.eval
  } getOrElse 1f
}

case class Con(value: Float) extends Exp {
  override def eval(implicit symbols: Map[Symbol, Float]): Float = {
    value
  }
}

case class Var(name: Symbol) extends Exp {
  override def eval(implicit symbols: Map[Symbol, Float]): Float = {
    symbols(name)
  }
}
