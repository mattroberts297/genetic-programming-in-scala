package model

import scala.collection.immutable._
import scala.util.Try

sealed trait Exp {
  def eval(implicit symbols: Map[Symbol, Float]): Float
}

trait BinOp {
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
