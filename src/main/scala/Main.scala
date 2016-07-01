
import scala.annotation.tailrec
import scala.collection.immutable._
import scala.collection.mutable
import org.slf4s.Logging

import scala.util.Random

import model._

object Main extends App with Logging {
  val population = 10000
  val maxDepth = 10
  val constants = Constants.range(-5f, 5f, 1f)
  val variables = IndexedSeq(Var('x))
  val functions = IndexedSeq(Add, Sub, Div, Mul)
  def pow(a: Float, b: Float): Float = Math.pow(a, b).toFloat
//  val expected = (-1f).to(1f, 0.05f).map(x => (Map('x -> x), pow(x, 2) - x - 2))
  val expected = (-3f).to(3f, 0.05f).map(x => (Map('x -> x), pow(x,3) / 4 + 3 * pow(x, 2) / 4 - 3 * x / 2 - 2))
  val terminalSet = constants ++ variables
  val functionSet = functions
  val trees = GP.rampHalfHalf(population, maxDepth, functionSet, terminalSet).toVector
  def criteria(fitness: Float): Boolean = fitness < 0.01f
  val fitTree = GP.run(trees, expected, criteria)
  log.debug(s"Fittest tree: ${fitTree}")
  log.debug("expected\t\tactual")
  expected.foreach { case (symbols, expected) =>
    log.debug(s"${expected}\t${fitTree.eval(symbols)}")
  }
}

object GP extends Logging {
  @tailrec
  def retrying[T](thunk: => Option[T]): T = {
    thunk match {
      case Some(t) => t
      case None => retrying(thunk)
    }
  }

  def run(
      initial: IndexedSeq[Exp],
      expected: Seq[(Map[Symbol, Float], Float)],
      criteria: Float => Boolean,
      maxRuns: Int = 1000): Exp = {
    @tailrec
    def loop(run: Int, current: IndexedSeq[Exp]): Exp = {
      val treesAndFitness = current.map(tree => tree -> fitness(tree, expected))
      val sortedTreesAndFitness = treesAndFitness.sortBy { case (_, fitness) => fitness }
      val sortedTrees = sortedTreesAndFitness.map { case (tree, _) => tree }
      val (topTree, minFitness) = sortedTreesAndFitness.head
      if (criteria(minFitness) || run == maxRuns) {
        topTree
      } else {
        // todo: mutation
        // for some reason as time passes you get more duplicates.
        val set = mutable.Set.empty[Exp]
        val replicas = sortedTrees.take(current.length / 100 * 19)
        replicas.foreach(exp => set += exp)
        val mutants = 1.to(current.length / 100).map { _ =>
          sortedTrees(Random.nextInt(sortedTrees.length))
        }.map(e => mutate(e))
        mutants.foreach(exp => set += exp)
        while (set.size < current.length) {
          set += crossover(
            tournament(random(treesAndFitness), random(treesAndFitness)),
            tournament(random(treesAndFitness), random(treesAndFitness)))

        }
        val replicasAndCrossovers = set.toVector
        log.debug(s"run=${run}, minFitness=${minFitness}, distinct=${current.distinct.length} crossovers.length=${replicasAndCrossovers.length}, replicas.length=${replicas.length}")
        loop(run + 1, replicasAndCrossovers)
      }
    }
    loop(1, initial)
  }

  def fitness(tree: Exp, expected: Seq[(Map[Symbol, Float], Float)]): Float = {
    expected.foldLeft(0f) { case (acc, (symbols, expected)) =>
      acc + Math.pow((expected - tree.eval(symbols)), 2).toFloat
    }
  }

  def tournament(a: (Exp, Float), b: (Exp, Float)): Exp = {
    val (aExp, aFit) = a
    val (bExp, bFit) = b
    if (aFit < bFit) aExp else bExp
  }

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
    @tailrec
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

  def crossover(left: Exp, right: Exp): Exp = {
    val replacement = biasedRandom(left)
    val target = biasedRandom(right)
    replace(right, target, replacement)
  }

  def mutate(exp: Exp): Exp = {
    val functions = Main.functionSet
    val terminals = Main.terminalSet
    val depth = Main.maxDepth
    val target = random(exp)
    val replacement = grow(depth, functions, terminals)
    replace(exp, target, replacement)
  }

  def replace(exp: Exp, target: Exp, replacement: Exp): Exp = {
    def repl(exp: Exp) = replace(exp, target, replacement)
    exp match {
      case exp: Exp if (exp.eq(target)) => replacement
      case Con(value) => Con(value)
      case Var(symbol) => Var(symbol)
      case Add(lhs, rhs) => Add(repl(lhs), repl(rhs))
      case Sub(lhs, rhs) => Sub(repl(lhs), repl(rhs))
      case Mul(lhs, rhs) => Mul(repl(lhs), repl(rhs))
      case Div(lhs, rhs) => Div(repl(lhs), repl(rhs))
    }
  }

  def collect(tree: Exp): IndexedSeq[Exp] = {
    def collect(acc: IndexedSeq[Exp], subtree: Exp): IndexedSeq[Exp] = {
      subtree match {
        case v: Var => IndexedSeq(v) ++ acc
        case c: Con => IndexedSeq(c) ++ acc
        case o: BinOp => IndexedSeq(o) ++ collect(acc, o.lhs) ++ collect(acc, o.rhs)
      }
    }
    collect(IndexedSeq.empty, tree)
  }

  def biasedCollect(tree: Exp): IndexedSeq[Exp] = {
    val ops = collectOps(tree)
    if (rand() > 0.9 || ops.isEmpty) {
      collectTerminals(tree)
    } else {
      ops
    }
  }

  def collectTerminals(tree: Exp): IndexedSeq[Exp] = {
    def collect(acc: IndexedSeq[Exp], subtree: Exp): IndexedSeq[Exp] = {
      subtree match {
        case v: Var => IndexedSeq(v) ++ acc
        case c: Con => IndexedSeq(c) ++ acc
        case o: BinOp => collect(acc, o.lhs) ++ collect(acc, o.rhs)
      }
    }
    collect(IndexedSeq.empty, tree)
  }

  def collectOps(tree: Exp): IndexedSeq[Exp with BinOp] = {
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

  def random(tree: Exp): Exp = {
    random(collect(tree))
  }

  def biasedRandom(tree: Exp): Exp = {
    random(biasedCollect(tree))
  }

  def random[T](elements: IndexedSeq[T]): T = {
    elements(Random.nextInt(elements.length))
  }

  def random[T](elements: Seq[T], probs: Seq[Float]): T = {
    val random = rand()
    var cumProb = 0f
    val cumProbs = probs.map { p =>
      cumProb = cumProb + p
      cumProb
    }
    elements.zip(cumProbs).find { case (_, p) =>
      p > random
    }.map { case (e, _) =>
      e
    }.getOrElse(elements.last)
  }

  def rand(): Float = Random.nextFloat()
}

object Constants {
  def range(from: Float, to: Float, step: Float): IndexedSeq[Con] = {
    IndexedSeq(from.to(to, step).map(Con): _*)
  }
}
