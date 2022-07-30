package cats.dataTypes

import cats.Eval

object PlayEval {
  // Eval is a data type for controlling synchronous evaluation.

  def main(argv: Array[String]) {
    val lazyEval = Eval.later {
      println("Running lazy with memoization calculation...")
      1 + 2 * 3
    }
    val eager = Eval.now {
      println("Running eager calculation...")
      1 + 2 * 3
    }
    val always = Eval.always {
      println("Running lazy  calculation...")
      1 + 2 * 3
    }
    lazyEval.value
    always.value
    lazyEval.value
    always.value
  }
}