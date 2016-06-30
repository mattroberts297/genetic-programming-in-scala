
import org.scalatest.{Matchers, WordSpec}

import scala.collection.immutable._
import scala.collection.mutable

class CollectSpec extends WordSpec with Matchers {
  "collect(root).head should be root" in {
    val root = Sub(Sub(Mul(Var('x), Var('x)), Var('x)), Con(2))
    GP.collect(root).head should be(root)
  }
}
