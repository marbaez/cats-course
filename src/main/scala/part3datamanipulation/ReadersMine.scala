package part3datamanipulation

object ReadersMine {

  case class Configuration(dbUsername: String, dbPassword: String, host: String, port: Int, nThreads: Int, emailReplyTo: String)
  case class DbConnection(username: String, password: String) {
    def getOrderStatus(orderId: Long): String = "Dispatched"
    def getLastOrderId(username: String): Long = 32144
  }
  case class HttpService(host: String, port: Int) {
    def start(): Unit = println("server started")
  }

  // bootstrap
  val configuration = Configuration("marbaez", "superpassword", "localhost", 8080, 8, "marcos@lostpixel.com")
  // cats Reader
  import cats.data.Reader
  val dbReader: Reader[Configuration, DbConnection] = Reader(conn => DbConnection(conn.dbUsername, conn.dbPassword))
  val dbConnection: DbConnection = dbReader.run(configuration)

  // Reader[I, O]
  val marcosOrderStatusReader: Reader[Configuration, String] = dbReader.map( dbCon => dbCon.getOrderStatus(55))
  val marcosOrderStatus: String = marcosOrderStatusReader.run(configuration)

  def getLastOrderStatus(username: String): String = {
    val userLastOrderIdReader: Reader[Configuration, String] = dbReader
      .map(_.getLastOrderId(username))
      .flatMap(lastOrderId => dbReader.map(_.getOrderStatus(lastOrderId)))

    // identical
    val userOrderStatusFor = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      lastOrderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
    } yield lastOrderStatus

    //userLastOrderIdReader.run(configuration)
    userOrderStatusFor.run(configuration)
  }

  // TODO: email service exercise
  case class EmailService(replyTo: String) {
    def sendEmail(address: String, contents: String): String = s"From: $replyTo To: $address >>> $contents"
  }

  val emailReader: Reader[Configuration, EmailService] = Reader(con => EmailService(con.emailReplyTo))

  def emailUser(username: String, userMail: String): String = {
    val sendedEmailReader = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      lastOrderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
      sendedEmail <- emailReader.map(_.sendEmail(userMail, s"Your last order status is $lastOrderStatus"))
    } yield sendedEmail

    sendedEmailReader.run(configuration)
  }


  def main(args: Array[String]): Unit = {
    println(emailUser("Marcos", "marcos@lostpixel.com"))
  }

}
