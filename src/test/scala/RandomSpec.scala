
import org.scalatest.{Matchers, WordSpec}

import scala.collection.immutable._
import scala.collection.mutable

class RandomSpec extends WordSpec with Matchers {
  val elements = Seq(1,2,3)
  val probabilities = Seq(0.25f, 0.25f, 0.5f)
  "random should work" in {
    val n = 100000
    val results = 1.to(n).map(_ => Trees.random(elements, probabilities))
    results.count(_ == 1).toFloat / n should be (0.25f +- 0.01f)
    results.count(_ == 2).toFloat / n should be (0.25f +- 0.01f)
    results.count(_ == 3).toFloat / n should be (0.5f +- 0.01f)
  }
}
