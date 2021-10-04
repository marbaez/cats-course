package part3datamanipulation

object FunctionalStateMine {

  //In Scala, a state is a data structure that describes the evolution during a period of time

  // State -> Answer
  type MyState[S, A] = S => (S, A)

  import cats.data.State
  val countAndSay: State[Int, String] = State(currentCount => (currentCount+ 1, s"Counted $currentCount"))
  val (eleven, counted10) = countAndSay.run(10).value
  // state = "iterative" computations

  // iterative version
  var a = 10
  a += 1
  val firstComputation = s"Added 1 to 10, obtained $a"
  a *= 5
  val secondComputation = s"Multiplied by 5, obtained $a"

  // pure FP with states
  val firstTransformation = State((s: Int) => (s + 1, s"Added 1 to 10, obtained ${s + 1}"))
  val secondTransformation = State((s: Int) => (s * 1, s"Multiplied by 5, obtained ${s * 5}"))
  val compositeTransformation: State[Int, (String, String)] = firstTransformation.flatMap { firstResult =>
    secondTransformation.map(secondResult => (firstResult, secondResult))
  }
  val compositeTransformation2 = for {
  firstResult <- firstTransformation
  secondResult <- secondTransformation
  } yield (firstResult, secondResult)

  // TODO: an online store
  case class ShoppingCart(items: List[String], total: Double)
  def addToCart(item: String, price: Double): State[ShoppingCart, Double] = {
    State( (s: ShoppingCart) => (ShoppingCart(item :: s.items, price + s.total), price + s.total))
  }

  val marcosCart: State[ShoppingCart, Double] = for {
    _ <- addToCart("Fender guitar", 500)
    _ <- addToCart("elixir strings", 19)
    total <- addToCart("Electric cable", 8)
  } yield total

  // TODO 2: Pure Gimnastics
  def inspect[A,B](f: A => B): State[A, B]  = State((s: A) => (s, f(s)))

  def get[A]: State[A,A] = State((s:A) => (s,s))

  def set[A](value: A): State[A, Unit] = State((s:A) => (value,()))

  def modify[A](f: A => A): State[A, Unit] = State((s:A) => (f(s),()))

  // prior methods availabla at

  import cats.data.State._

  for {
    a <- get[Int]
    _ <- set[Int](a + 10)
    b <- get[Int]
    _ <- modify[Int](_ + 43)
    c <- inspect[Int, Int](_ * 42)
  } yield (a, b, c)


  def main(args: Array[String]): Unit = {
    println(compositeTransformation.run(10).value)
    println(marcosCart.run(ShoppingCart(List(), 0)).value)
  }

}
