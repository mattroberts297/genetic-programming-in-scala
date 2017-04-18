## Genetic programming in scala

### Introduction

I hope that one day software engineers will obsolete ourselves. That is, we will write an application capable of writing all other applications for us. Genetic programming (GP) is about just that: creating programs that solve problems based on expected outputs for given inputs. As a colleague of mine put it, you write the unit tests and the computer does the rest. Of course, both my colleague and I know we're not quite there yet!

Most, if not all, of the algorithms implemented here are based on the excellent descriptions in _A Field Guide to Genetic Programming_. I've concentrated on readability and correctness as opposed to speed of execution or even reliability, so please excuse the lacklustre performance and the fact that tail recursion is not used in places. Also note that the algorithms implemented here are, to quote the authors, "the simplest thing that could possibly work".

The snippets (those without file names) below should work in the SBT console of the project (or an IntelliJ worksheet). The project is available in my GitHub repository: [mattroberts297/genetic-programming-1](http://github.com/mattroberts297/genetic-programming-1).

### Representing programs

There are three main steps in GP (2 and 3 are repeated):

1. Generate a population of programs;
2. Evaluate the fitness of those programs; and
3. Evolve a new, hopefully fitter, population of programs.

In order to do any of those things, however, we need to be able to represent programs in some way. This is normally done by means of an abstract syntax tree (AST). The AST considered here is handcrafted and only capable of representing simple polynomial expressions. There are, however, many tools capable of automatically generating ASTs from grammar files (my personal favourite being ANTLR). Here is the handcrafted AST:

###### model/Exp.scala

```scala
package model

sealed trait Exp

case class Con(value: Float) extends Exp

case class Var(name: Symbol) extends Exp

sealed trait BinOp extends Exp {
  def lhs: Exp
  def rhs: Exp
}

case class Add(lhs: Exp, rhs: Exp) extends BinOp

case class Sub(lhs: Exp, rhs: Exp) extends BinOp

case class Mul(lhs: Exp, rhs: Exp) extends BinOp

case class Div(lhs: Exp, rhs: Exp) extends BinOp

object Exp {
  import scala.util.Try
  def eval(exp: Exp, st: ST): Float = exp match {
    case Con(value) => value
    case Var(name) => st(name)
    case Add(lhs, rhs) => eval(lhs, st) + eval(rhs, st)
    case Sub(lhs, rhs) => eval(lhs, st) - eval(rhs, st)
    case Mul(lhs, rhs) => eval(lhs, st) * eval(rhs, st)
    case Div(lhs, rhs) => Try(eval(lhs, st) / eval(rhs, st)).getOrElse(1f)
  }
}
```

And a type alias from ST to Map[Symbol, Float] for the symbol table:

###### model/package.scala

```scala
import scala.collection.immutable.Map

package object model {
  type ST = Map[Symbol, Float]
}
```

There are a three design decisions in the above code worth discussing.

First, constants and variables are always of type `Float`. There are two main reasons for this: type safety and memory consumption. Creating a generic AST and accompanying symbol table(s) would be reasonably straight forward, but writing GP algorithms that operate on them in a type safe fashion would not be so simple.

Second, the `eval` method never throws `java.lang.ArithmeticException: / by zero` when performing division returning `1` instead. This ensures both type and evaluation safety and, again, makes it easier to write GP algorithms that operate on ASTs without encountering runtime exceptions.

Third, and finally, both `Exp` and `BinOp` are sealed traits. This makes pattern matching easier because the compiler will warn us about incomplete matches instead of leaving us at the mercy of runtime exceptions.

#### Example ASTs

In order to better understand what we are trying to achieve automatically it is useful to manually create some ASTs and plot the results of evaluating them for various values of `x`.

##### Second degree polynomial

Here is an AST for the second degree polynomial `f(x) = x^2 - x - 2`:

```scala
import model._

Sub(Sub(Mul(Var('x), Var('x)), Var('x)), Con(2))
```

It can be evaluated for various values of `x` like so:

```scala
import model._

val exp = Sub(Sub(Mul(Var('x), Var('x)), Var('x)), Con(2))
Exp.eval(exp, Map('x -> -3)) // res0: Float = 10
Exp.eval(exp, Map('x -> 0)) // res0: Float = -2
Exp.eval(exp, Map('x -> 3)) // res0: Float = 4
```

This plots shows the result of the expression for values of `x` between `-3` and `3`:

![Second degree polynomial](https://assets.mattroberts.io/img/SecondDegreePolynomial.png "Second degree polynomial")

##### Third degree polynomial

Here is another, slightly more complex, AST for the third degree polynomial `f(x) = x^3 / 4 + 3x^2 / 4 - 3x / 2 - 2`:

```scala
import model._

Sub(Sub(Add(Div(Mul(Var('x),Mul(Var('x),Var('x))),Con(4)),Div(Mul(Con(3),Mul(Var('x),Var('x))),Con(4))),Div(Mul(Con(3),Var('x)),Con(2))),Con(2))
```

As before, the result for various values of `x` can be determined like so:

```scala
import model._

val exp = Sub(Sub(Add(Div(Mul(Var('x),Mul(Var('x),Var('x))),Con(4)),Div(Mul(Con(3),Mul(Var('x),Var('x))),Con(4))),Div(Mul(Con(3),Var('x)),Con(2))),Con(2))
Exp.eval(exp, Map('x -> -2)) // res1: Float = 2.0
Exp.eval(exp, Map('x -> 0)) // res2: Float = -2.0
Exp.eval(exp, Map('x -> 2)) // res3: Float = 0.0
```

And here is the plot:

![Third degree polynomial](https://assets.mattroberts.io/img/ThirdDegreePolynomial.png "Third degree polynomial")

From looking at both the AST and the chart you can hopefully see that the third degree polynomial will be more difficult to generate automatically than the second degree polynomial. These two expressions, one simpler and one more complex, will make good test cases for our GP solution.

### Generating programs

In GP, the leafs of an AST, whether they be constants or variables, are referred to as the terminal set. The branches whether they be unary, binary or some other arity function are referred to as the function set. Using the AST defined above the following terminal and functional sets can be created:

```scala
import scala.collection.immutable._

val terminalSet = IndexedSeq(Var('x)) ++ 1f.to(5f, 1f).map(Con)
val functionSet = IndexedSeq(Add, Sub, Div, Mul)
```

With these sets it's possible to generate an AST of some arbitrary depth `depth`. The simplest way to do this is to create a `full` tree. The algorithm is reasonably simple. If the depth has been reached then return a random terminal from the terminal set. Otherwise return a random function from the function set and use the result of the next recursion as its arguments. Here is a function that does that:

###### GP.scala
```scala
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

def random[T](elements: IndexedSeq[T]): T = {
  elements(Random.nextInt(elements.length))
}
```

The `full` function resides in an object named `GP`. I omit the following "boiler plate" when discussing methods in that object:

###### GP.scala

```scala
import scala.annotation.tailrec
import scala.collection.immutable._
import scala.util.Random
import org.slf4s.Logging
import model._
object GP with Logging {
  // Functions go here.
}
```

Here are some example ASTs with depths `1`, `2` and `3` generated with the `full` method:

![Full trees](https://assets.mattroberts.io/img/FullTrees.png "Full trees")

The `full` method is simple and definitely works, but all the trees for a given depth have the exactly same number of leafs. This leads to a surprisingly uniform population. The `grow` method resolves this issue by randomly stopping the recursion:

###### GP.scala
```scala
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

def random(): Float = Random.nextFloat()
```

Here are three example ASTs all of depth `3` generated with `grow` method:

![Grow trees](https://assets.mattroberts.io/img/GrowTrees.png "Grow trees")

These two methods can be composed together to create the method `rampHalfHalf`. The idea behind this is to use `grow` for one half of the population and `full` for the other. Further, the method starts at `depth` 1 and ramps up to some `maxDepth` (as opposed to all trees being the same `depth`):

###### GP.scala
```scala
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
```

The method above is a little finicky for several reasons. First, instead of comparing `acc.size` to `count` the value `i` is used. This is because the `size` method on set is inefficient. Second, if the `acc` set already contains a tree then `i` is not incremented. Third, and contrary to `i`, `depth` is always incremented unless the `maxDepth` has been reached and in this case it is set to `1`. This is because, especially for large values of `count`, it is possible that all permutations of trees at a certain `depth` have been generated. If `depth` were not incremented then the recursion could be infinite.

> In fact, even the above method would not terminate if `count` were high and `maxDepth` low, so be careful if you use it. If termination is important to you, and it probably is, then it's best to accept duplicates and return a `Seq` instead.

With the above, it is now possible to create an initial population:

```scala
val count = 10000
val maxDepth = 5
val terminalSet = IndexedSeq(Var('x)) ++ 1f.to(5f, 1f).map(Con)
val functionSet = IndexedSeq(Add, Sub, Div, Mul)
val initial = rampHalfHalf(count, maxDepth, functionSet, terminalSet).toVector
```

### Evaluating fitness

Evaluating fitness is easier than you might think. Given some input there is an expected output. Then evaluating fitness is just a matter of measuring the difference between the expected and actual outputs produced by evaluating a function. For anyone who has done unit testing with multiple test cases this should feel familiar. The next step, namely reducing those differences into one measure for all test cases, may feel decidely unfamiliar. For anyone with a machine learning background this will feel quite natural though.

In our example the inputs are stored in the symbol table. The symbol table therefore is the only input. We also know how to calculate the result of the polynomial expression, so we can calculate the expected values for a given set of inputs:

```scala
val cases = (-1f).to(1f, 0.05f).map(x => (Map('x -> x), pow(x, 2) - x - 2)).toMap
```

The type of `cases` is `Map[ST, Float]`. In other words, it maps symbol tables (inputs) to expected results. Now we need to use these cases to evaluate the fitness of a given tree. To do this, first we have to evaluate each case against the tree and store the absolute difference between the actual and expected results. Then we have to sum these differences to give one final figure for all cases:

###### GP.scala
```scala
def fitness(cases: Map[ST, Float])(tree: Exp): Float = {
  cases map { case (symbols, expected) =>
    val actual = Exp.eval(tree, symbols)
    Math.abs(expected - actual)
  } reduce { (a, b) =>
    a + b
  }
}
```

I have intentional written this using map and reduce. I could have also used `foldLeft` or `foldRight` and an accumulator. The map-reduce nature of the algorithm should feel natural to anyone from a machine learning background and hints at the fact that this can be trivially parallelised.

You may have already guessed this, but as the correctness of the generated trees improves the value returned by the `fitness` function will approach `0`. When the tree gives the correct result for each case then the fitness function will return `0`:

```scala
import scala.collection.immutable._
import model._
import GP._

def pow(a: Float, b: Float): Float = Math.pow(a, b).toFloat
val exp = Sub(Sub(Mul(Var('x), Var('x)), Var('x)), Con(2))
val cases = (-1f).to(1f, 0.05f).map(x => (Map('x -> x), pow(x, 2) - x - 2)).toMap
val f = fitness(cases)(exp)
f // 0.0
```

### Evolving programs

The next step is to evolve the initial population into a new population. There are many algorithms to evolve programs (some haven't even been discovered yet). I'll look at three:

1. Mutation
2. Crossover
3. Replication

#### Mutation

Mutation will introduce a degree of randomness and brute force into the overall approach. Most GP enthusiasts would recommend keeping it's use to a minimum (and I agree). That said, it is important because it can help bump a GP run out of a local optimum. The idea is to take a tree, pick a random subtree within it's branches and replace it with another tree grown from the function and terminal sets:

###### GP.scala
```scala
  def mutate(
      functionSet: IndexedSeq[(Exp, Exp) => Exp],
      terminalSet: IndexedSeq[Exp],
      maxDepth: Int)(
      exp: Exp): Exp = {
    val target = random(exp)
    val replacement = grow(maxDepth, functionSet, terminalSet)
    replace(exp, target, replacement)
  }
```

Note the use of two new functions `random` and `replace` and one old function `grow`. The first function invokes `collectAll` to convert the `Exp` into an `IndexedSeq[Exp]` containing the root of the tree and all its subtrees using the generic `collect` function and a, not so partial, anonymous function. The result is then passsed to the previously defined `random` function that takes an `IndexedSeq[T]`:

###### GP.scala
```scala  
  def random(tree: Exp): Exp = {
    random(collectAll(tree))
  }

  def collectAll(tree: Exp): IndexedSeq[Exp] = {
    collect(tree) { case e => e }
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
```

The second function is responsible for replacing some node `target` in the original expression `exp` with a `replacement`. The method uses reference equality instead of value equality because there may be many subtrees with value equality. This works because the `mutate` method picks an element from the original tree. The method performs a recursive descent through the tree. The first `case` in the `match` statement checks for reference equality and if it is found then returns the `replacement`. All other cases perform a copy. If the target does not exist then a copy of the original expression is returned:

###### GP.scala
```scala
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
```

#### Crossover and Tournament

##### Crossover

Crossover is responsible for breeding new trees. It does this by taking two parents (`left` and `right`) and selecting random subtree from each (`replacement` and `target`). The random selection is actually quite heavily biased in favour of operators to avoid pruning trees to aggressively. A copy of the `right` tree is made with the `target` replaced with the `replacement`:

###### GP.scala
```scala
  def crossover(left: Exp, right: Exp): Exp = {
    val replacement = biasedRandom(left)
    val target = biasedRandom(right)
    replace(right, target, replacement)
  }

  def biasedRandom(tree: Exp): Exp = {
    random(biasedCollect(tree))
  }

  def biasedCollect(tree: Exp): IndexedSeq[Exp] = {
    val ops = collectOps(tree)
    if (random() > 0.9 || ops.isEmpty) {
      collectTerminals(tree)
    } else {
      ops
    }
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
```

##### Tournament

Tournament is the process by which two parents are selected for crossover. The method takes two trees and their respective fitness values and returns the fittest tree:

###### GP.scala
```scala
def tournament(a: (Exp, Float), b: (Exp, Float)): Exp = {
  val (aExp, aFit) = a
  val (bExp, bFit) = b
  if (aFit < bFit) aExp else bExp
}
```

#### Replication

The last algorithm is very simple. Given a population, take a percentage of the fittest individuals and carry them across to the next population. The method to implement replication will have to take a population of trees as it's input. Note that up until now each algorithm has taken one or two trees as inputs. With that in mind, here is the `replicas` function:

###### GP.scala
```scala
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
```

#### Crossover and mutation again

Before putting everything together, let's create methods that wrap `mutate` and `crossover`, but take populations of trees as their arguments. Starting with `crossovers`:

###### GP.scala
```scala
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
```

And finally the `mutants`:

###### GP.scala
```scala
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
```

One thing to note is that each method takes an accumulator. This avoids the situation whereby the population is compromised of trees that are exactly the same.

### Finishing touches

The `run` method uses a tight loop to repeatedly evolve the initial population until some user-defined fitness criteria is reached:

###### GP.scala
```scala
def run(
    functionSet: IndexedSeq[(Exp, Exp) => Exp],
    terminalSet: IndexedSeq[Exp],
    cases: Map[ST, Float],
    fitness: Map[ST, Float] => Exp => Float,
    criteria: Float => Boolean,
    maxRuns: Int = 1000,
    maxDepth: Int = 6,
    populationSize: Int = 100000): Exp = {
  val initial = rampHalfHalf(populationSize, maxDepth, functionSet, terminalSet).toVector

  @tailrec
  def loop(run: Int, current: IndexedSeq[Exp]): Exp = {
    val treesAndFitness = current.map { tree => tree -> fitness(cases)(tree) }
    val (topTree, minFitness) = fittest(treesAndFitness)
    val mutants = GP.mutants(functionSet, terminalSet, maxDepth)_
    log.debug(s"run=${run}, minFitness=${minFitness}")
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
              Set.empty))).toIndexedSeq)
    }
  }
  loop(1, initial)
}

def fittest(treesAndFitness: IndexedSeq[(Exp, Float)]): (Exp, Float) = {
  treesAndFitness.minBy { case (_, fitness) => fitness }
}
```

The `Main` application contains key parameters including:

- `populationSize` The number of trees to generate.
- `maxDepth` The maximum depth of the initial tree population.
- `terminalSet` The terminals to use when constructing trees.
- `functionSet` The functions to use when constructing trees.
- `cases` The cases to use when evaluating the fitness of trees.
- `criteria` The criteria used to decide when to stop evolving trees.

###### Main.scala
```scala
import scala.collection.immutable._
import org.slf4s.Logging
import model._

object Main extends App with Logging {
  import GP._
  val maxDepth = 6
  val terminalSet = IndexedSeq(Var('x)) ++ 1f.to(5f, 1f).map(Con)
  val functionSet = IndexedSeq(Add, Sub, Div, Mul)
  def pow(a: Float, b: Float): Float = Math.pow(a, b).toFloat
  val cases = (-3f).to(3f, 0.5f).map(x => (Map('x -> x), pow(x, 2) - x - 2)).toMap
  def criteria(fitness: Float): Boolean = fitness < 0.01f
  val fitTree = run(functionSet, terminalSet, cases, fitness, criteria, populationSize = 1000)
  log.info(s"Fittest tree: ${fitTree}")
  log.info("expected\t\tactual")
  cases.foreach { case (symbols, expected) =>
    log.info(s"${expected}\t${Exp.eval(fitTree, symbols)}")
  }
}
```

If you're interested to see what a single evolution looks like then you can do that from the console or a worksheet:

```scala
import scala.collection.immutable._
import model._
import GP._

val count = 10
val maxDepth = 6
val terminalSet = IndexedSeq(Var('x)) ++ 1f.to(5f, 1f).map(Con)
val functionSet = IndexedSeq(Add, Sub, Div, Mul)
val initial = rampHalfHalf(count, maxDepth, functionSet, terminalSet).toVector
def pow(a: Float, b: Float): Float = Math.pow(a, b).toFloat
val cases = (-1f).to(1f, 0.05f).map(x => (Map('x -> x), pow(x, 2) - x - 2)).toMap
val treesAndFitness = initial.map { tree => tree -> fitness(cases)(tree) }
val mutants = GP.mutants(functionSet, terminalSet, maxDepth)_
val next = crossovers(
  treesAndFitness,
  0.8f,
  mutants(
    initial,
    0.01f,
    replicas(
      treesAndFitness,
      0.19f,
      Set.empty))).toIndexedSeq
```

### Convergence

#### Second degree polynomial

The `Main` object and worksheet above contain `cases` for the second degree polynomial introduced way back at the beginning of this post. As the number of runs increase the aim is to converge on the actual solution. If we chart the fittest tree of each iteration then we can see this happening run by run:

![Second degree polynomial run 1](https://assets.mattroberts.io/img/SecondDegreePolynomialRun1.png "Second degree polynomial run 1")


![Second degree polynomial run 2](https://assets.mattroberts.io/img/SecondDegreePolynomialRun2.png "Second degree polynomial run 2")


![Second degree polynomial run 3](https://assets.mattroberts.io/img/SecondDegreePolynomialRun3.png "Second degree polynomial run 3")


![Second degree polynomial run 4](https://assets.mattroberts.io/img/SecondDegreePolynomialRun4.png "Second degree polynomial run 4")


![Second degree polynomial run 5](https://assets.mattroberts.io/img/SecondDegreePolynomialRun5.png "Second degree polynomial run 5")

Note that the actual line is obscured by the expected line i.e. it is a perfect match. Also, I labelled the charts as runs 1 through 5, but they are actually just runs where there was an improvement i.e. more runs were needed. That said, convergence seemed to happen pretty quickly all things considered i.e. sometimes sub second and no more than 2 seconds or 20 runs.

#### Third degree polynomial

To try and generate a third degree polynomial I just had to change my `cases`:

```scala
val cases = (-3f).to(3f, 0.5f).map(x => (Map('x -> x), pow(x,3) / 4 + 3 * pow(x, 2) / 4 - 3 * x / 2 - 2)).toMap
```

I suggested earlier that a third degree polynomial would be more difficult to generate automatically. In fact, what I found is that I would be lucky if the program converged at all. As time passed and the number of runs increased beyond a 100 then things started to slow down. Further, the fitness decrements became smaller and more infrequent. The give away for me was the runs slowing down. I took an educated guess and assumed the trees were getting too big and then the program became stuck in a local, but large optima (I could have proven this by printing out some trees).

##### Pruning

The solution, as with the other algorithms, is reasonably straight forward: prune the trees after each evolution. I'm sure this is probably in the book, but I either skimmed over it or forgot it. Here is the method I used:

```scala
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
```

And here is how I tacked it onto the end of the loop (note that I was too lazy to write a version that operates on a population of trees):

```scala
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
```

Doing this constrains the search space and means that each execution is quick i.e. no slow down over time. With pruning I found the number of runs varied from 50-200, but that the time to execute floated around the 2 second mark on average. Here are some more charts:

![Third degree polynomial run 1](https://assets.mattroberts.io/img/ThirdDegreePolynomialRun1.png "Third degree polynomial run 1")

![Third degree polynomial run 3](https://assets.mattroberts.io/img/ThirdDegreePolynomialRun3.png "Third degree polynomial run 3")

![Third degree polynomial run 5](https://assets.mattroberts.io/img/ThirdDegreePolynomialRun5.png "Third degree polynomial run 5")

![Third degree polynomial run 8](https://assets.mattroberts.io/img/ThirdDegreePolynomialRun8.png "Third degree polynomial run 8")

![Third degree polynomial run 10](https://assets.mattroberts.io/img/ThirdDegreePolynomialRun10.png "Third degree polynomial run 10")

### Final thoughts

Genetic programming seems like a brilliant way to search a constrained space for an optimum or approximate solution to some problem. Don't forget the full source is available on GitHub at [mattroberts297/genetic-programming-1](http://github.com/mattroberts297/genetic-programming-1). If there is interest then I'll do a follow up post showing how to run this on a Spark cluster.
