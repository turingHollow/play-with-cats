package cats.typeClasses


import cats.{Applicative, ApplicativeError, MonadError}
import cats.implicits._

object PlayMonadError {

  def getCityClosestToCoordinate[F[_]](x: (Int, Int))(implicit ae: Applicative[F]): F[String] = {
    ae.pure("Minneapolis, MN")
  }

  def getTemperatureByCity[F[_]](city: String)(implicit ae:  Applicative[F]): F[Int] = {
    ae.pure(78)
  }

  def getTemperatureByCoordinates[F[_] : MonadError[*[_], String]](x: (Int, Int)): F[Int] = {
    for {c <- getCityClosestToCoordinate[F](x)
         t <- getTemperatureByCity[F](c)} yield t
  }

  def main(argv: Array[String]) {
    // can work with various effects
    println(getTemperatureByCoordinates[Either[String, *]](44 -> 93)
    )
  }


}
