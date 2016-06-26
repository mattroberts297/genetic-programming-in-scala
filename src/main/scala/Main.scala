
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
//  val tree = Add(Add(Mul(Var('x), Var('x)), Var('x)), Con(1))
//
//  log.debug(s"Tree: $tree")
//  log.debug(s"Tree.eval(x -> 0): ${tree.eval(Map('x -> 0))}")
//  log.debug(s"Tree.eval(x -> 0.5): ${tree.eval(Map('x -> 0.5f))}")
//  log.debug(s"Tree.eval(x -> 1): ${tree.eval(Map('x -> 1))}")
//  log.debug(s"Tree.eval(x -> 2): ${tree.eval(Map('x -> 2))}")
//
//  val leftTree = Trees.full(3, functions, variables ++ constants)
//  log.debug(s"leftTree: $leftTree")
//  val rightTree = Trees.full(3, functions, variables ++ constants)
//  log.debug(s"rightTree: $rightTree")
//  log.debug(s"crossover: ${Trees.crossover(leftTree, rightTree)}")

  def loop(run: Int, trees: Seq[Exp], expected: Seq[(Map[Symbol, Float], Float)]): Exp = {
    log.debug(s"Run $run")
    val treesAndFitness = trees.map(tree => (tree, Trees.fitness(tree, expected)))
    val sorted = treesAndFitness.sortBy { case (_, fitness) => fitness }
    val (topTree, topFitness) = sorted.head
    if (topFitness < 0.1f || run == 10000) {
      topTree
    } else {
      val sortedTrees = sorted.map { case (tree, _) =>
        tree
      }
      val bestQuarter = sortedTrees.take(sortedTrees.length / 4)
      val newTreesA = bestQuarter.map { tree =>
        Trees.crossover(tree, bestQuarter(Random.nextInt(bestQuarter.length)))
      }
      val newTreesB = bestQuarter.map { tree =>
        Trees.crossover(bestQuarter(Random.nextInt(bestQuarter.length)), tree)
      }
      val oldTrees = bestQuarter ++ sortedTrees.takeRight(sortedTrees.length / 4)
      loop(run + 1, newTreesA ++ newTreesB ++ oldTrees, expected)
    }
  }

  val expected = (-1f).to(1f, 0.05f).map(x => (Map('x -> x), x * x + x + 1))
  val population = 1000
  val terminalSet = constants ++ variables
  val functionSet = functions
  val firstTrees = 1.to(population).map(_ => Trees.full(5, functionSet, terminalSet))
  val fitTree = loop(1, firstTrees, expected)
  log.debug(s"Fit tree: ${fitTree}")
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

  def crossover(left: Exp, right: Exp): Exp = {
    val lefts = collect(left)
    val rights = collectOps(right)
    val leftSubtree = lefts(Random.nextInt(lefts.length))
    val rightSubtree = rights(Random.nextInt(rights.length))
    rightSubtree match {
      case Add(_, rhs) => Add(leftSubtree, rhs)
      case Sub(_, rhs) => Sub(leftSubtree, rhs)
      case Mul(_, rhs) => Mul(leftSubtree, rhs)
      case Div(_, rhs) => Div(leftSubtree, rhs)
    }
  }

  private def collect(tree: Exp): IndexedSeq[Exp] = {
    def collect(acc: IndexedSeq[Exp], subtree: Exp): IndexedSeq[Exp] = {
      subtree match {
        case v: Var => IndexedSeq(v) ++ acc
        case c: Con => IndexedSeq(c) ++ acc
        case o: BinOp =>
          IndexedSeq(o) ++ collect(acc, o.lhs) ++ collect(acc, o.rhs)
      }
    }
    collect(IndexedSeq.empty, tree)
  }

  private def collectOps(tree: Exp): IndexedSeq[BinOp] = {
    def collectOps(acc: IndexedSeq[BinOp], subtree: Exp): IndexedSeq[BinOp] = {
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
      acc + Math.abs(expected - tree.eval(symbols))
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
