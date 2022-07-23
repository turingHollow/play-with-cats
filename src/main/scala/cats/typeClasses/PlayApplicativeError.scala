
import cats.{ApplicativeError, Monad}
import cats.data.Validated
import cats.implicits._

object PlayApplicativeError {

  def attemptDivideApplicativeErrorWithMap2[F[_]](x: Int, y: Int)(implicit ae: ApplicativeError[F, String]): F[_] = {
    if (y == 0) ae.raiseError("divisor is error")
    else {
      val fa = ae.pure(x)
      val fb = ae.pure(y)
      ae.map2(fa, fb)(_ / _)
    }
  }

  type OnError[A] = Either[String, A]
  type MyValidated[A] = Validated[String, A]

  def attemptDivideApplicativeErrorAbove2[F[_]](x: Int, y: Int)(implicit ae: ApplicativeError[F, String]): F[Int] =
    if (y == 0) ae.raiseError("Bad Math")
    else if (y == 1) ae.raiseError("Waste of Time")
    else ae.pure(x / y)

  def handler[F[_]](f: F[Int])(implicit ae: ApplicativeError[F, String]): F[Int] = {
    ae.handleError(f) {
      case "Bad Math" => -1
      case "Waste of Time" => -2
      case _ => -3
    }
  }

  def ifF[F[_]: Monad, A](f: F[A])(implicit F: ApplicativeError[F, String]): F[_] = {
    f.flatMap(value =>
    F.ifF(Monad[F].pure(value.isInstanceOf[Int]))(123, 456)
    )
  }

  def main(argv: Array[String]) {
    // can work with various effects
    println(attemptDivideApplicativeErrorWithMap2[OnError](30, 10))
    println(attemptDivideApplicativeErrorWithMap2[Validated[String, *]](30, 10))
    println(handler(attemptDivideApplicativeErrorAbove2(0, 0)))
    println(ifF(attemptDivideApplicativeErrorAbove2(456, 234)))
  }

}
