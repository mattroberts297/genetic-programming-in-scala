
import scala.annotation.tailrec
import scala.collection.immutable._
import scala.util.Random

import model._
import org.slf4s.Logging


// todo consider pruning large trees
object GP extends Logging {
  def run(
    functionSet: IndexedSeq[(Exp, Exp) => Exp],
    terminalSet: IndexedSeq[Exp],
    cases: Map[ST, Float],
    fitness: Map[ST, Float] => Exp => Float,
    criteria: Float => Boolean,
    maxRuns: Int = 1000,
    maxDepth: Int = 5,
    populationSize: Int = 100000): Exp = {
    val initial = rampHalfHalf(populationSize, maxDepth, functionSet, terminalSet).toVector

    @tailrec
    def loop(run: Int, current: IndexedSeq[Exp], lastMinFitness: Float = Float.MaxValue): Exp = {
      val treesAndFitness = current.map {  tree => tree -> fitness(cases)(tree) }
      val (topTree, minFitness) = fittest(treesAndFitness)
      val mutants = GP.mutants(functionSet, terminalSet, maxDepth)_
      val prune = GP.prune(terminalSet, maxDepth)_
      log.debug(s"run=${run}, minFitness=${minFitness}")
      if (minFitness < lastMinFitness) {
        println(s"Fitter tree")
        println("x,expected,actual")
        cases.toVector.map { case (symbols, expected) =>
          (symbols.head._2, expected, Exp.eval(topTree, symbols))
        } sortBy { case (x, _, _) =>
          x
        } foreach { case (x, e, a) =>
          println(s"$x,$e,$a")
        }
      }
      if (criteria(minFitness)) {
        topTree
      } else {
        loop(
          run + 1,
          crossovers(
            treesAndFitness,
            0.8f,
            mutants(
              current,
              0.01f,
              replicas(
                treesAndFitness,
                0.19f,
                Set.empty))).toIndexedSeq.map(prune), minFitness)
      }
    }
    loop(1, initial)
  }

  def prune(
      terminalSet: IndexedSeq[Exp],
      maxDepth: Int)(
      tree: Exp): Exp = {
    def loop(subtree: Exp, depth: Int): Exp = {
      subtree match {
        case op: BinOp if (depth >= maxDepth) => random(terminalSet)
        case Con(value) => Con(value)
        case Var(symbol) => Var(symbol)
        case Add(lhs, rhs) => Add(loop(lhs, depth + 1), loop(rhs, depth + 1))
        case Sub(lhs, rhs) => Sub(loop(lhs, depth + 1), loop(rhs, depth + 1))
        case Mul(lhs, rhs) => Mul(loop(lhs, depth + 1), loop(rhs, depth + 1))
        case Div(lhs, rhs) => Div(loop(lhs, depth + 1), loop(rhs, depth + 1))
      }
    }
    loop(tree, 0)
  }

  def fittest(treesAndFitness: IndexedSeq[(Exp, Float)]): (Exp, Float) = {
    treesAndFitness.minBy { case (_, fitness) => fitness }
  }

  def replicas(
    treesAndFitness: IndexedSeq[(Exp, Float)],
    percent: Float = 0.19f,
    acc: Set[Exp] = Set.empty[Exp]): Set[Exp] = {
    val length = acc.size + (treesAndFitness.length.toFloat * percent).toInt
    val sortedTrees = treesAndFitness.
      sortBy { case (_, fitness) => fitness }.
      map { case (exp, _) => exp }
    @tailrec
    def loop(acc: Set[Exp], remaining: IndexedSeq[Exp]): Set[Exp] = {
      if (acc.size == length) {
        acc
      } else {
        loop(acc + remaining.head, remaining.tail)
      }
    }
    loop(acc, sortedTrees)
  }

  def mutants(
    functionSet: IndexedSeq[(Exp, Exp) => Exp],
    terminalSet: IndexedSeq[Exp],
    maxDepth: Int)(
    trees: IndexedSeq[Exp],
    percent: Float = 0.01f,
    acc: Set[Exp]= Set.empty[Exp]): Set[Exp] = {
    val length = acc.size + (trees.length.toFloat * percent).toInt
    def mutate = GP.mutate(functionSet, terminalSet, maxDepth)_
    @tailrec
    def loop(acc: Set[Exp]): Set[Exp] = {
      if (acc.size == length) {
        acc
      } else {
        loop(acc + mutate(random(trees)))
      }
    }
    loop(acc)
  }

  def crossovers(
    treesAndFitness: IndexedSeq[(Exp, Float)],
    percent: Float = 0.8f,
    acc: Set[Exp]= Set.empty[Exp]): Set[Exp] = {
    val length = acc.size + (treesAndFitness.length.toFloat * percent).toInt
    @tailrec
    def loop(acc: Set[Exp]): Set[Exp] = {
      if (acc.size == length) {
        acc
      } else {
        loop(acc + crossover(
          tournament(random(treesAndFitness), random(treesAndFitness)),
          tournament(random(treesAndFitness), random(treesAndFitness))))
      }
    }
    loop(acc)
  }

  def full(
    depth: Int,
    functions: IndexedSeq[(Exp, Exp) => Exp],
    terminals: IndexedSeq[Exp]): Exp = {
    def loop(i: Int): Exp = {
      if (i == depth) {
        random(terminals)
      } else {
        random(functions)(loop(i + 1), loop(i + 1))
      }
    }
    loop(0)
  }

  def grow(
    depth: Int,
    functions: IndexedSeq[(Exp, Exp) => Exp],
    terminals: IndexedSeq[Exp]): Exp = {
    def randomStop: Boolean = {
      val tl = terminals.length.toFloat
      val fl = functions.length.toFloat
      random() < tl / (tl + fl)
    }
    def loop(i: Int): Exp = {
      if (i == depth || randomStop) {
        random(terminals)
      } else {
        random(functions)(loop(i + 1), loop(i + 1))
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

  def fitness(cases: Map[ST, Float])(tree: Exp): Float = {
    cases map { case (symbols, expected) =>
      val actual = Exp.eval(tree, symbols)
      Math.abs(expected - actual)
    } reduce { (a, b) =>
      a + b
    }
  }

  def tournament(a: (Exp, Float), b: (Exp, Float)): Exp = {
    val (aExp, aFit) = a
    val (bExp, bFit) = b
    if (aFit < bFit) aExp else bExp
  }

  def crossover(left: Exp, right: Exp): Exp = {
    val replacement = biasedRandom(left)
    val target = biasedRandom(right)
    replace(right, target, replacement)
  }

  def mutate(
    functionSet: IndexedSeq[(Exp, Exp) => Exp],
    terminalSet: IndexedSeq[Exp],
    maxDepth: Int)(
    exp: Exp): Exp = {
    val target = random(exp)
    val replacement = grow(maxDepth, functionSet, terminalSet)
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

  def collect[T](tree: Exp)(pf: PartialFunction[Exp, T]): IndexedSeq[T] = {
    def loop(subtree: Exp, acc: IndexedSeq[T]): IndexedSeq[T] = {
      val result = if (pf.isDefinedAt(subtree)) acc :+ pf(subtree) else acc
      subtree match {
        case v: Var => result
        case c: Con => result
        case o: BinOp => result ++ loop(o.lhs, acc) ++ loop(o.rhs, acc)
      }
    }
    loop(tree, IndexedSeq.empty[T])
  }

  def collectAll(tree: Exp): IndexedSeq[Exp] = {
    collect(tree) { case e => e }
  }

  def collectOps(tree: Exp): IndexedSeq[Exp] = {
    collect(tree) { case o: BinOp => o }
  }

  def collectTerminals(tree: Exp): IndexedSeq[Exp] = {
    collect(tree) {
      case v: Var => v
      case c: Con => c
    }
  }

  def biasedCollect(tree: Exp): IndexedSeq[Exp] = {
    val ops = collectOps(tree)
    if (random() > 0.9 || ops.isEmpty) {
      collectTerminals(tree)
    } else {
      ops
    }
  }

  def biasedRandom(tree: Exp): Exp = {
    random(biasedCollect(tree))
  }

  def random(): Float = Random.nextFloat()

  def random(tree: Exp): Exp = {
    random(collectAll(tree))
  }

  def random[T](elements: IndexedSeq[T]): T = {
    elements(Random.nextInt(elements.length))
  }

  def random[T](elements: Seq[T], probs: Seq[Float]): T = {
    val rand = random()
    var cumProb = 0f
    val cumProbs = probs.map { p =>
      cumProb = cumProb + p
      cumProb
    }
    elements.zip(cumProbs).find { case (_, p) =>
      p > rand
    }.map { case (e, _) =>
      e
    }.getOrElse(elements.last)
  }
}
