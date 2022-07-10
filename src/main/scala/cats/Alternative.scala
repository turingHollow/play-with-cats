import cats._
import cats.implicits._

object PlayAlternative {

  trait Decoder[A] {
    def decode(in: String): Either[Throwable, A]
  }

  object Decoder {
    def from[A](f: String => Either[Throwable, A]): Decoder[A] =
      new Decoder[A] {
        def decode(in: String) = f(in)
      }
  }

  implicit val decoderAlternative = new Alternative[Decoder] {
    def pure[A](a: A) = Decoder.from(Function.const(Right(a)))

    def empty[A] = Decoder.from(Function.const(Left(new Error("No dice."))))

    def combineK[A](l: Decoder[A], r: Decoder[A]): Decoder[A] =
      new Decoder[A] {
        def decode(in: String) = l.decode(in).orElse(r.decode(in))
      }

    def ap[A, B](ff: Decoder[A => B])(fa: Decoder[A]): Decoder[B] =
      new Decoder[B] {
        def decode(in: String) = fa.decode(in) ap ff.decode(in)
      }
  }

  def parseInt(s: String): Either[Throwable, Int] = Either.catchNonFatal(s.toInt)
  def parseIntFirstChar(s: String): Either[Throwable, Int] = Either.catchNonFatal(2 * Character.digit(s.charAt(0), 10))

  // Try first parsing the whole, then just the first character.
  val decoder: Decoder[Int] = Decoder.from(parseInt _) <+> Decoder.from(parseIntFirstChar _)

  def requestResource(a: Int): Either[(Int, String), (Int, Long)] = {
    if (a % 4 == 0) Left((a, "Bad request"))
    else if (a % 3 == 0) Left((a, "Server error"))
    else Right((a, 200L))
  }

  val result: (Vector[(Int, String)], Vector[(Int, Long)]) = ((requestResource _).pure[Vector] ap Vector(5, 6, 7, 99, 1200, 8, 22)).separate

  def main(argv: Array[String]) {
    println(Alternative[Vector].empty[Int])
    println(5.pure[Vector])
    println(7.pure[Vector] <+> 8.pure[Vector])
    val double: Int => Int = _ * 2
    val addFive: Int => Int = _ + 5
    println(double.pure[Vector] <+> addFive.pure[Vector] ap Vector(5, 8))
    println(decoder.decode("555"))
    println(decoder.decode("5a"))
    println(((requestResource _).pure[Vector] ap Vector(5, 6, 7, 99, 1200, 8, 22)).separate)
  }
}
