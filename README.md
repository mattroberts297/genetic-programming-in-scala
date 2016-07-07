## Genetic programming in scala

### Introduction

I hope that one day software engineers will obsolete ourselves. That is, we will write an application capable of writing all other applications for us. Genetic programming (GP) is about just that: creating programs that solve problems based on expected outputs for given inputs. As a colleague of mine put it, you write the unit tests and the computer does the rest.

Most, if not all, of the algorithms implemented here are based on the excellent descriptions in _A Field Guide to Genetic Programming_. I've concentrated on readability and correctness as opposed to speed of execution or even reliability, so please excuse the lacklustre performance and the fact that tail recursion is not used in places. Also note that the algorithms implemented here are, to quote the authors, "the simplest thing that could possibly work".

### Representing programs

There are three main steps in GP:

1. Generating a population of programs;
2. Evaluating the fitness of those programs; and
3. Evolving a new, hopefully fitter, population of programs.

In order to do any of those things we need to be able to represent programs in some way. This is normally done by means of an abstract syntax tree (AST). The AST considered here is handcrafted and only capable of representing simple polynomial expressions. There are, however, many tools capable of automatically generating ASTs from grammar files (my personal favourite being ANTLR). Here is the handcrafted AST:

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
import scala.collection.immutable._

package object model {
  type ST = Map[Symbol, Float]
}
```

There are a three design decisions in the above code worth discussing.

First, constants and variables are always `Float`. There are two main reasons for this: type safety and memory consumption. Creating a generic AST and accompanying symbol table(s) would be reasonably straight forward, but writing GP algorithms that operate on the AST in a type safe fashion would not be so simple.

Second, the `eval` method never throws `java.lang.ArithmeticException: / by zero` when performing division returning `1` instead. This ensures both type and evaluation safety and, again, makes it easier to write GP algorithms that operate on ASTs without encountering runtime exceptions.

Third, and finally, both `Exp` and `BinOp` are sealed traits. This makes pattern matching easier because the compiler will warn us about incomplete matches instead of leaving us at the mercy of runtime exceptions.

### Example ASTs

In order to better understand what we are trying to achieve automatically it is useful to manually create some ASTs and plot the results of evaluating them for various values of `x`.

#### Second degree polynomial

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

![Second degree polynomial](SecondDegreePolynomial.png "Second degree polynomial")

#### Third degree polynomial

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

![Third degree polynomial](ThirdDegreePolynomial.png "Third degree polynomial")

From looking at both the AST and the chart you can hopefully see that the third degree polynomial will be more difficult to arrive at automatically than the second degree polynomial. These two expressions, one simpler and one more complex, will make good test cases for our GP algorithms.

### Generating a population of programs

In GP, the leafs of an AST, whether they be constants or variables, are referred to as the terminal set. The branches whether they be unary, binary or some other arity function are referred to as the function set. So for the AST defined above the sets are as follows:

```scala


```

