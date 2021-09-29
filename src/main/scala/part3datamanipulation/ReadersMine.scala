package part3datamanipulation

object ReadersMine {

  case class Configuration(dbUsername: String, dbPassword: String, host: String, port: Int, nThreads: Int)
  case class DbConnection(username: String, password: String) {
    def getOrderStatus(orderId: Long): String = "Dispatched"
  }
  case class HttpService(host: String, port: Int) {
    def start(): Unit = println("server started")
  }

  // bootstrap



  def main(args: Array[String]): Unit = {

  }

}
