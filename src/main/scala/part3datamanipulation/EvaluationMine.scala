package part3datamanipulation

object EvaluationMine {

  /*
    Cats makes distintction between:
      - evaluating an expression eagerly
      - evaluating lazily and every time you request it
      - evaluating lazily and keeping the value (memoizing)
   */

  import cats.Eval
  val instantEval: Eval[Int] = Eval.now {
    println("Computing now!")
    2345
  }

  val redoEval: Eval[Int] = Eval.always {
    println("Computing always!")
    2048
  }

  val delayedEval = Eval.later {
    println("Evaluating later!")
    4096
  }

  // TODO 2: impliement defer such that  defer(Eval.now) does not run the side effects
  def defer[T](eval: => Eval[T]): Eval[T] = Eval.defer(eval)

  def main(args: Array[String]): Unit = {
    defer(Eval.now{
      println("Now!")
      42
    })
  }

}
