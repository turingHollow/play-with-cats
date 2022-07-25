package cats.dataTypes

import cats.arrow.FunctionK
import cats.~>
import cats.implicits._

object PlayFunctionK {

  //A FunctionK transforms values from one first-order-kinded type
  // (a type that takes a single type parameter, such as List or Option) into another first-order-kinded type

  val first: FunctionK[List, Option] = new FunctionK[List, Option] {
    def apply[A](l: List[A]): Option[A] = l.headOption
  }

  val firstWithKindProjector: List ~> Option = λ[List ~> Option](_.headOption)

  type ErrorOr[_] = Either[String, _]

  val errorOrFirst: FunctionK[List, ErrorOr] =
    λ[FunctionK[List, ErrorOr]](_.headOption.toRight("ERROR: the list was empty!"))

  val fa = List(1)
  val f = (x: Int) => x * 3

  def main(argv: Array[String]) {
    println(first(fa.map(f)))
    // fk(F.map(fa)(f)) <-> G.map(fk(fa))(f)
    println(first(fa).map(f))
    println(errorOrFirst(fa))

  }
}
