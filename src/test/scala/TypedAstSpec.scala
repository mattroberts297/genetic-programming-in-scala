
import scala.collection.immutable._
import scala.collection.mutable

class TypedAstSpec {
  type ST[T] = Map[Symbol, T]
  trait Exp[T] {
    def eval(implicit st: ST[T]): T
  }
}
