package cats.dataTypes

import cats.data.State

object PlayState {

  // State[S, A] is basically a function S => (S, A)
  //  where S is the type that represents your state and A is the result the function produces

  final case class Robot(
                          id: Long,
                          sentient: Boolean,
                          name: String,
                          model: String)

  final case class Seed(long: Long) {
    def next = Seed(long * 6364136223846793005L + 1442695040888963407L)
  }

  val nextLong: State[Seed, Long] = State(seed =>
    (seed.next, seed.long))

  val nextBoolean: State[Seed, Boolean] = nextLong.map(long =>
    long >= 0)

  val createRobot: State[Seed, Robot] =
    for {
      id <- nextLong
      sentient <- nextBoolean
      isCatherine <- nextBoolean
      name = if (isCatherine) "Catherine" else "Carlos"
      isReplicant <- nextBoolean
      model = if (isReplicant) "replicant" else "borg"
    } yield Robot(id, sentient, name, model)

  def main(argv: Array[String]) {

    println(nextLong.run(Seed(1L)).value)

    println(createRobot.runA(Seed(2L)).value)
  }

}
