package part3datamanipulation

import cats.Semigroup

import scala.annotation.tailrec
import scala.util.Try

object DataValidationMine {

  import cats.data.Validated
  // Acts as an Either

  val aValidValue: Validated[String, Int] = Validated.valid(42) //Right value in Either
  val anInvalidValue: Validated[String, Int] = Validated.invalid("Something went wrong") // left value
  val aTest: Validated[String, Int] = Validated.cond(42  > 39, 99, "meaning of life is too small")

  // TODO: Use Either
  /*
    - n must me a prime
    - n must be non-negative
    - n <= 100
    - n must be even
   */
  def testPrime(n: Int) = {
    @tailrec
    def tailrecPrime(d: Int): Boolean = {
      if (d <= 1) true
      else n % d != 0 && tailrecPrime(d-1)
    }
    if ( n==0 ) false
    else tailrecPrime(Math.abs(n / 2))
  }

  def testNumber(n: Int): Either[List[String], Int] = {
    val isNotEven: List[String] = if (n % 2 == 0) List() else List("n must be even")
    val isNonNegative: List[String] = if (n > 0) List() else List("n must be non negative")
    val isLess100: List[String] = if (n < 100) List() else List("n must be less than 100")
    val isNonPrime: List[String] = if (testPrime(n)) List() else List("n must be less than 100")

    if ((n % 2 == 0) && n > 0 && n < 100 && testPrime(n)) Right(n)
    else Left(isLess100 ++ isNonPrime ++ isNonNegative ++ isNotEven)
  }
  import cats.instances.list._
  implicit val combineIntMax: Semigroup[Int] = Semigroup.instance[Int](Math.max)
  def validateNumber(n: Int): Validated[List[String], Int] =
    Validated.cond(n % 2  == 0, n, List("Number must be even"))
      .combine(Validated.cond(n >= 0, n , List("Number must be non-negative")))
      .combine(Validated.cond(n < 100, n , List("Number must be less than 100")))
      .combine(Validated.cond(testPrime(n), n , List("Number must be a prime")))

  // chain
  aValidValue.andThen(_ => anInvalidValue)
  //test a valid value
  aValidValue.ensure(List("something went wrong"))(_ % 2 == 0 )
  //transform
  aValidValue.map(_ + 1)
  aValidValue.leftMap(_.length)
  aValidValue.bimap(_.length, _ + 1)

  // interoperate with stdlib
  val eitherToValidated: Validated[List[String], Int] = Validated.fromEither(Right(42))
  val optionToVal: Validated[List[String], Int] = Validated.fromOption(None, List("Nothing present here"))
  val tryToValidated: Validated[Throwable, Int] = Validated.fromTry(Try("something".toInt))
  // backwards
  aValidValue.toOption
  aValidValue.toEither

  // TODO 2: form validation
  object FormValidation {
    type FormValitation[T] = Validated[List[String], T]

    /*

     */
  }

  def main(args: Array[String]): Unit = {

  }
}
