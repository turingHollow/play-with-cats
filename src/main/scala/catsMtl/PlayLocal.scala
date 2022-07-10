import cats._
import cats.data._
import cats.implicits._
import cats.mtl._

object PlayLocal {

  def calculateContentLength[F[_] : Applicative](implicit F: Ask[F, String]): F[Int] =
    F.ask.map(_.length)

  def calculateModifiedContentLength[F[_] : Applicative](implicit F: Local[F, String]): F[Int] =
    F.local(calculateContentLength[F])("Prefix " + _)

  val result = calculateModifiedContentLength[Reader[String, *]].run("Hello")


  def both[F[_] : Monad](implicit F: Local[F, String]): F[(Int, Int)] = for {
    length <- calculateContentLength[F]
    modifiedLength <- calculateModifiedContentLength[F]
  } yield (length, modifiedLength)

  def main(argv: Array[String]) {
    println(both[Reader[String, *]].run("Hello"))
  }
}
