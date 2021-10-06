package part4typeclasses

import cats.Semigroup
import cats.data.Validated

object SemigroupalMine {

  trait MySemigroupal[F[_]] {
    def product[A, B](fa:F[A], fb:F[B]): F[(A,B)]
  }

  import cats.Semigroupal
  import cats.instances.option._  // implicit Semigroupal[Option]
  val optionSemigroupal = Semigroupal[Option]
  val aTupledoption = optionSemigroupal.product(Some(123), Some("a string")) // Some((123, "a string))
  val aNoneTupled = optionSemigroupal.product(Some(123), None) // None

  import cats.instances.list._
  val aTupledList = Semigroupal[List].product(List(1,2), List("a", "b"))

  //TODO: implement product with monads
  import cats.Monad
  def productWithMondas[F[_], A, B](fa: F[A], fb: F[B])(implicit monad: Monad[F]): F[(A,B)] =
    monad.flatMap(fa)(a => monad.map(fb)(b => (a,b)))

  // Monads extends Semigroupal

  // Use case for Semigroupal
  type ErrorsOR[T] = Validated[List[String], T]
  val validatedSemigroupal = Semigroupal[ErrorsOR]

  def main(args: Array[String]): Unit = {
    println(aTupledList)
  }

}
