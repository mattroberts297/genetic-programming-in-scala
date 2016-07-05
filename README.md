## Genetic programming in scala

### Introduction

I hope that one day software engineers will obsolete ourselves. That is, we will write an application capable of writing all other applications for us. Genetic programming (GP) is about just that: creating programs that solve problems based on expected outputs for given inputs. As a colleague of mine put it, you write the unit tests and the computer does the rest. 

Most, if not all, of the algorithms implemented here are based on the excellent descriptions in _A Field Guide to Genetic Programming_. I've concentrated on readability and correctness as opposed to speed of execution or even reliability, so please excuse the lacklustre performance and the fact that tail recursion is not used in places. Also note that the algorithms implemented here are, to quote the authors, "the simplest thing that could possibly work".

### Getting started

There are three main steps in GP:

1. Generating a population of programs;
2. Evaluating the fitness of those programs; and
3. Evolving a new, hopefully fitter, population of programs.

In order to do any of those things we need to be able to represent programs in some way. This is normally done by means of an abstract syntax tree (AST). The AST considered here is handcrafted and only capable of representing simple polynomial expressions. There are, however, many tools capable of automatically generating ASTs from grammar files (my personal favourite being ANTLR). Here is the handcrafted AST:

###### model/Exp.scala

```scala
package model

sealed trait Exp {
  def eval(st: ST): Float
}

case class Con(value: Float) extends Exp {
  override def eval(st: ST): Float = value
}

case class Var(name: Symbol) extends Exp {
  override def eval(st: ST): Float = st(name)
}

sealed trait BinOp extends Exp {
  def lhs: Exp
  def rhs: Exp
}

case class Add(lhs: Exp, rhs: Exp) extends BinOp {
  override def eval(st: ST): Float = lhs.eval(st) + rhs.eval(st)
}

case class Sub(lhs: Exp, rhs: Exp) extends BinOp {
  override def eval(st: ST): Float = lhs.eval(st) - rhs.eval(st)
}

case class Mul(lhs: Exp, rhs: Exp) extends BinOp {
  override def eval(st: ST): Float = lhs.eval(st) * rhs.eval(st)
}

case class Div(lhs: Exp, rhs: Exp) extends BinOp {
  import scala.util.Try
  override def eval(st: ST): Float = Try {
    lhs.eval(st) / rhs.eval(st)
  } getOrElse 1f
}
```

###### model/package.scala

```scala
import scala.collection.immutable.Map

package object model {
  type ST = Map[Symbol, Float]
}
```

There are a few design decisions worth discussing.

First, the `eval` method of constants, variables and binary operators always returns `Float`. There are two main reasons for this: type safety and memory consumption. Creating a generic AST would be reasonably straight forward, but writing GP algorithms that operate on the AST in a type safe manner would not be so simple.

Second, division never throws `java.lang.ArithmeticException: / by zero` returning `1` instead. This ensures both type and evaluation safety and again makes it easier to write GP algorithms that operate on ASTs without encountering runtime exceptions.

Third, and finally, both `Exp` and `BinOp` are sealed traits. This makes pattern matching easier because the compiler will warn us about incomplete matches instead of leaving us at the mercy of runtime exceptions.

### Generating a population of programs


In GP, the leafs of an AST, whether they be constants or variables, are referred to as the terminal set. The branches whether they be unary, binary or some other arity function are referred to as the function set.

