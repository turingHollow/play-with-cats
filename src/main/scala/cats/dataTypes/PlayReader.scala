package cats.dataTypes

import cats.data.Reader


object PlayReader {

  // basically type Reader[-A, B] = ReaderT[Id, A, B] AND type ReaderT[F[_], -A, B] = Kleisli[F, A, B]

  case class Course(desc: String, code: String)

  class AuthService {
    def isAuthorised(userName: String): Boolean = userName.startsWith("J")
  }

  class CourseService {
    def register(course: Course,
                 isAuthorised:
                 Boolean,
                 name: String) = {
      if (isAuthorised)
        s"User $name registered for the course: ${course.code}"
      else
        s"User: $name is not authorised to register for course: ${course.code}"
    }
  }

  case class CourseManager(course: Course,
                           userName: String,
                           authService: AuthService,
                           courseService: CourseService)

  def isAuthorised = Reader[CourseManager, Boolean] { courseMgr =>
    courseMgr.authService.isAuthorised(courseMgr.userName)
  }

  def register(isFull: Boolean) = Reader[CourseManager, String] { courseMgr =>
    courseMgr.courseService.register(courseMgr.course,
      isFull,
      courseMgr.userName)
  }

  val result = for {
    authorised <- isAuthorised
    response <- register(authorised)
  } yield response

  val course = Course("Computer Science", "CS01")
  val courseManager = CourseManager(course, "Jon", new AuthService, new CourseService)

  def main(argv: Array[String]) {
    println(result.run(courseManager))
  }

}
