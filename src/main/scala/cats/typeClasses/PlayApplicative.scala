import cats._
import cats.data.Nested
import cats.implicits._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object PlayApplicative {
  //Applicative encodes working with multiple independent effects

  def product3[F[_] : Applicative, A, B, C](fa: F[A], fb: F[B], fc: F[C]): F[(A, B, C)] = {
    val F = Applicative[F]
    val fabc = F.product(F.product(fa, fb), fc)
    F.map(fabc) { case ((a, b), c) => (a, b, c) }
  }

  val f: (Int, Char) => Double = (i, c) => (i + c).toDouble
  val int: Option[Int] = Some(5)
  val char: Option[Char] = Some('a')
  val functionInContext: Option[Char => Double] = int.map(i => (c: Char) => f(i, c))

  // applicative compose
  val x: Future[Option[Int]] = Future.successful(Some(5))
  val y: Future[Option[Char]] = Future.successful(Some('a'))

  def main(argv: Array[String]) {
    // applicative for effect management
    println(product3(Option(1), Option("two"), Option(2.0)))
    // app method
    println(Applicative[Option].ap(functionInContext)(char))
    // applicative compose
    println(Applicative[Future].compose[Option].map2(x, y)(_ + _))
    // nested
    println(Applicative[Nested[Future, Option, *]].map2(Nested(x), Nested(y))(_ + _))
    // mapN
    println(Applicative[Option].map3(Option(1), Option("2"), Option(2.0))
    ((x: Int, y: String, z: Double) => x + y.toInt + z.toInt))
    // tupleN
    println(Applicative[Option].tuple3(Option(1), Option("2"), Option(2.0)))
  }
}