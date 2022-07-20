package cats.dataTypes

import cats.data.Validated.Valid
import cats.data.{Nested, Validated}

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import cats.implicits._


object PlayNested {
  // Nested can help with this by composing the two map operations into one
  val nested: Nested[Option, Validated[String, *], Int] = Nested(Some(Valid(123)))

  case class UserInfo(name: String, age: Int)
  case class User(id: String, name: String, age: Int)

  def createUser(userInfo: UserInfo): Future[Either[List[String], User]] =
    Future.successful(Right(User("user 123", userInfo.name, userInfo.age)))

  def createUsersNested(userInfos: List[UserInfo]): Future[Either[List[String], List[User]]] =
    userInfos.traverse(userInfo => Nested(createUser(userInfo))).value

  val userInfos = List(
    UserInfo("Alice", 42),
    UserInfo("Bob", 99)
  )

  def main(argv: Array[String]) {
    println(nested.map(a => a - 5).value)
    println(Await.result(userInfos.traverse(createUser), 1.second))
    println(Await.result(createUsersNested(userInfos), 1.second))
  }
}
