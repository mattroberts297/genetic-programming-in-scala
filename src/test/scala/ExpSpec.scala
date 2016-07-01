
import org.scalatest.{Matchers, WordSpec}

import scala.collection.immutable._
import scala.collection.mutable

import model._

class ExpSpec extends WordSpec with Matchers {
  "exp.eval should evaluate correctly" in {
    // x * x - x - 2
    val exp = Sub(Sub(Mul(Var('x), Var('x)), Var('x)), Con(2))
    exp.eval(Map('x -> 1f)) should be(-2f +- 0.001f)
  }
}
