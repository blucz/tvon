package tvon.server

object App extends {
    def main(args: Array[String]) {
        val config = Config.load("config.json") match {
          case Left(config) => config 
          case Right(err)   => println(s"Error loading configuration: ${err}")
                               System.exit(1)
                               return
        }

        val manager = new Manager(config)
        manager.initialize()

        val server = new ApiServer(manager = manager, port = config.port)
        server.start()
        Log.info("Press enter to exit server")
        readLine()
        Log.info("Exiting...")
        manager.shutdown()
        server.stop()
    }
}