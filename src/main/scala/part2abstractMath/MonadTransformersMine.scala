package part2abstractMath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object MonadTransformersMine {

  def sumAllOptions(values: List[Option[Int]]): Int = ???

  // Option transformer
  import cats.data.OptionT
  import cats.instances.list._ // Fetch an implicit OptionT[List]

  val listOfNumberOptions: OptionT[List, Int] = OptionT(List(Option(1), Option(2)))
  val listOfCharOptions: OptionT[List, Char] = OptionT(List(Option('a'), Option('b'), Option.empty[Char]))
  val listOfTuples: OptionT[List, (Int, Char)] = for {
    char <- listOfCharOptions
    int <- listOfNumberOptions
  } yield (int, char)

  // Either transformer
  import cats.data.EitherT
  import cats.instances.future._
  val listOfEither: EitherT[List, String, Int] = EitherT(List(Left("somthing went wrong"), Right(42)))
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val futureOfEither: EitherT[Future, String, Int] = EitherT(Future(Right(45)))

  /*
  TODO exercise:

   */
  val bandWidths = Map(
    "server1.rockthejvm.com" -> 50,
    "server2.rockthejvm.com" -> 300,
    "server3.rockthejvm.com" -> 170
  )
  type AsyncResponse[T] = EitherT[Future, String, T] // wrapper over Future[Either[String, T]]

  def getBandwidth(server: String): AsyncResponse[Int] = bandWidths.get(server) match {
    case None => EitherT.left(Future(s"Server $server unreachable"))
    case Some(b) => EitherT.right(Future(b))
  }

  def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] = for {
    band1 <- getBandwidth(s1)
    band2 <- getBandwidth(s2)
  } yield band1 + band2 > 250

  def generateTrafficSpikeReport(s1: String, s2: String): AsyncResponse[String] = ???


  def main(args: Array[String]): Unit = {
    println(listOfTuples)
  }
}
