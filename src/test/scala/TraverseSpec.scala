
import org.scalatest.{Matchers, WordSpec}

import scala.collection.immutable._
import scala.collection.mutable

class TraverseSpec extends WordSpec with Matchers {
  "copy" should {
    "not return the same reference" in {
      val a = Add(Mul(Con(4), Con(2)), Sub(Con(10), Con(5)))
      val b = copy(a)
      a.eq(b) should be (false)
    }

    "return the same value" in {
      val a = Add(Mul(Con(4), Con(2)), Sub(Con(10), Con(5)))
      val b = copy(a)
      a == b should be (true)
    }
  }

  "replace" should {
    "replace exp with referential equality" in {
      val lhs = Add(Con(2), Con(2))
      val rhs = Add(Con(2), Con(2))
      val replacement = Mul(Con(2), Con(2))
      val exp = Mul(lhs, rhs)
      val result = replace(exp, lhs, replacement).asInstanceOf[Mul]
      result.lhs.eq(replacement) should be (true)
    }

    "not replace value equality" in {
      val lhs = Add(Con(2), Con(2))
      val rhs = Add(Con(2), Con(2))
      val replacement = Mul(Con(2), Con(2))
      val exp = Mul(lhs, rhs)
      val result = replace(exp, lhs, replacement).asInstanceOf[Mul]
      result.rhs.ne(replacement) should be (true)
      result.rhs == rhs should be (true)
    }
  }

  "reference equality (eq)" should {
    "return true for references" in {
      val a = Con(2)
      val b = a
      b.eq(a) should be (true)
    }

    "return false for different references" in {
      val a = Con(2)
      val c = Con(2)
      c.eq(a) should be (false)
    }
  }

  def copy(exp: Exp): Exp = {
    exp match {
      case Con(value) => Con(value)
      case Var(symbol) => Var(symbol)
      case Add(lhs, rhs) => Add(copy(lhs), copy(rhs))
      case Sub(lhs, rhs) => Sub(copy(lhs), copy(rhs))
      case Mul(lhs, rhs) => Mul(copy(lhs), copy(rhs))
      case Div(lhs, rhs) => Div(copy(lhs), copy(rhs))
    }
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
}
