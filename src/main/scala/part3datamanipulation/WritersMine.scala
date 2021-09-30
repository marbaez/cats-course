package part3datamanipulation

object WritersMine {

  import cats.data.Writer
  // 1. define them at start
  val aWriter: Writer[List[String], Int] = Writer(List("Started something"), 45)

  // 2. manipulate them with pure FP
  val anIncreasedWriter = aWriter.map(_ + 1) // value increases, logs stay the same
  val aLogsWriter = aWriter.mapWritten(_ :+ "found something interesting") //values stays the same, log changes
  //modify bboth at the same time
  val aWriterWithBoth = aWriter.bimap(_ :+ "found something interesting", _ + 1)
  val aWriterWithBoth2 = aWriter.mapBoth { (logs, value) =>
    ((logs :+ "Found something interesting"), value + 1)
  }

  //flatmap on writers
  import cats.instances.vector._
  val writerA = Writer(Vector("Log A1", "Log A2"), 10)
  val writerB = Writer(Vector("Log B1"), 10)
  val compositeWriter = for {
    va <- writerA
    vb <- writerB
  } yield va + vb

  // reset the logs
  import cats.instances.list._
  val anEmptyWriter = aWriter.reset  // clear the logs, keep the value

  // 3. dump either the value or the logs
  val desiredValue = aWriter.value
  val logs = aWriter.written
  val (l, v) = aWriter.run


  // TODO 1: rewrite a function which prints things with writers
  def countAndLog(n: Int): Writer[Vector[String], Int] = {
      if (n <= 0) {
        Writer(Vector("starting"), 0)
      } else {
        countAndLog(n - 1).bimap(_ :+ s"$n", _ + 1)
      }
  }

  // TODO 2:
  def sumWithLogs(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector(), 0)
    else for {
      _ <- Writer(Vector(s"Now at $n"), n)
      lowerSum <- sumWithLogs(n - 1)
      _ <- Writer(Vector(s"Computed sum(${n - 1}) = $lowerSum"), n)
    } yield lowerSum + n
  }

  def main(args: Array[String]): Unit = {
    println(compositeWriter.run)
    countAndLog(10).written.foreach(println)
    sumWithLogs(100).written.foreach(println)
  }

}
