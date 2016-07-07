import org.scalatest.{Matchers, WordSpec}
import scala.collection.immutable._
import model._

class ExpSpec extends WordSpec with Matchers {
  "f(x) = x^2 - x - 2 should evaluate correctly" in {
    // x * x - x - 2
    val second = Sub(Sub(Mul(Var('x), Var('x)), Var('x)), Con(2))
    Exp.eval(second, Map('x -> 1f)) should be(-2f +- 0.001f)
  }

  // f(x) = x^3 / 4 + 3x^2 / 4 - 3x / 2 - 2

  "x^2 should evaluate correctly" in {
    val x = Var('x)
    val x2 = Mul(x, x)
    Exp.eval(x2, Map('x -> 2f)) should be (4f +- 0.001f)
  }

  "x^3 should evaluate correctly" in {
    val x = Var('x)
    val x3 = Mul(x, Mul(x, x))
    Exp.eval(x3, Map('x -> 2f)) should be (8f +- 0.001f)
  }

  "3x^2 should evaluate correctly" in {
    val x = Var('x)
    val x2 = Mul(x, x)
    val threex2 = Mul(Con(3), x2)
    Exp.eval(threex2, Map('x -> 2f)) should be (12f +- 0.001f)
  }

  "x^3 / 4 + 3x^2 / 4 - 3x / 2 - 2 should evaluate correctly" in {
    val x = Var('x)
    val x2 = Mul(x, x)
    val x3 = Mul(x, Mul(x, x))
    val t1 = Div(x3, Con(4))
    val t2 = Div(Mul(Con(3), x2), Con(4))
    val t3 = Div(Mul(Con(3), x), Con(2))
    val t4 = Con(2)
    val third = Sub(Sub(Add(t1, t2), t3), t4)
    Exp.eval(third, Map('x -> 2f)) should be (0f +- 0.001f)
    Exp.eval(third, Map('x -> 0f)) should be (-2f +- 0.001f)
    Exp.eval(third, Map('x -> -2f)) should be (2f +- 0.001f)
  }
}
