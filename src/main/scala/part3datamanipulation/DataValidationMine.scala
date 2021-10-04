package part3datamanipulation

import scala.annotation.tailrec

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
    val isNonPrime: List[String] = if (testPrime()) List() else List("n must be less than 100")

    if ((n % 2 == 0) && n > 0 && n < 100 && testPrime(n)) Right(n)
    else Left(isLess100 ++ isNonPrime ++ isNonNegative ++ isNotEven)
  }

  def main(args: Array[String]): Unit = {

  }
}
