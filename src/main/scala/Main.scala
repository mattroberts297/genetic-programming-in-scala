
import scala.List
import scala.annotation.tailrec
import scala.collection.immutable._
import scala.collection.mutable

import org.slf4s.Logging
import scala.util.Random

import model._

case class InitParams(
  count: Int,
  maxDepth: Int,
  functions: IndexedSeq[(Exp, Exp) => Exp],
  terminals: IndexedSeq[Exp])

object Main extends App with Logging {
  import GP._
  import FloatMath._
  val count = 100000
  val maxDepth = 5
  val terminalSet = IndexedSeq(Var('x)) ++ 1f.to(5f, 1f).map(Con)
  val functionSet = IndexedSeq(Add, Sub, Div, Mul)
  val initParams = InitParams(count, maxDepth, functionSet, terminalSet)
  val cases = (-1f).to(1f, 0.05f).map(x => (Map('x -> x), pow(x, 2) - x - 2)).toMap
//  val cases = (-3f).to(3f, 0.05f).map(x => (Map('x -> x), pow(x,3) / 4 + 3 * pow(x, 2) / 4 - 3 * x / 2 - 2)).toMap
  def criteria(fitness: Float): Boolean = fitness < 0.01f
  val fitTree = run(functionSet, terminalSet, cases, fitness, criteria, populationSize = 1000)
  log.debug(s"Fittest tree: ${fitTree}")
  log.debug("expected\t\tactual")
  cases.foreach { case (symbols, expected) =>
    log.debug(s"${expected}\t${Exp.eval(fitTree, symbols)}")
  }
}

object FloatMath {
  def pow(a: Float, b: Float): Float = Math.pow(a, b).toFloat
}

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
    def loop(run: Int, current: IndexedSeq[Exp]): Exp = {
      val treesAndFitness = current.map { tree =>
        tree -> fitness(cases)(tree)
      }
      val sortedTreesAndFitness = treesAndFitness.sortBy { case (_, fitness) =>
        fitness
      }
      val sortedTrees = sortedTreesAndFitness.map { case (tree, _) => tree }
      val (topTree, minFitness) = sortedTreesAndFitness.head
      if (criteria(minFitness) || run == maxRuns) {
        topTree
      } else {
        val replicas = sortedTrees.take(current.length / 100 * 19)
//        replicas.foreach(exp => set += exp)
        val mutants = 1.to(current.length / 100).map { _ =>
          random(sortedTrees)
        }.map(e => mutate(functionSet, terminalSet, maxDepth)(e))
//        mutants.foreach(exp => set += exp)
        // todo make immutable
        val set = mutable.Set.empty[Exp]
        while (set.size < current.length / 100 * 80) {
          set += crossover(
            tournament(random(treesAndFitness), random(treesAndFitness)),
            tournament(random(treesAndFitness), random(treesAndFitness)))
        }
        val replicasAndCrossovers = set.toVector ++ replicas ++ mutants
        log.debug(s"run=${run}, minFitness=${minFitness}")
        loop(run + 1, replicasAndCrossovers)
      }
    }

    loop(1, initial)
  }

  def mutateAll(
      functionSet: IndexedSeq[(Exp, Exp) => Exp],
      terminalSet: IndexedSeq[Exp],
      maxDepth: Int)(
      trees: IndexedSeq[Exp],
      percent: Float = 0.1f,
      seen: Set[Exp]= Set.empty[Exp]): IndexedSeq[Exp] = {
    def mutateOne = mutate(functionSet, terminalSet, maxDepth)_
    def loop(acc: Set[Exp]): IndexedSeq[Exp] = {

      ???
    }
    ???
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

object Constants {
  def range(from: Float, to: Float, step: Float): IndexedSeq[Con] = {
    IndexedSeq(from.to(to, step).map(Con): _*)
  }
}
