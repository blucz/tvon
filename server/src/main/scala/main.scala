package tvon.server

trait Lifecycle {
  def init()     { }
  def shutdown() { }
}

object App extends {
    def main(args: Array[String]) {
        org.slf4j.LoggerFactory.getLogger("ROOT")
        val appconfig = Config.load("config.json") match {
          case Left(config) => config 
          case Right(err)   => println(s"Error loading configuration: ${err}")
                               System.exit(1)
                               return
        }

        trait ConfigComponentImpl extends ConfigComponent { val config = appconfig }
        var app = new  Lifecycle
                  with ConfigComponentImpl
                  with WebServerComponent
                  with StorageComponent
                  with BrowserComponent
                  with CollectionComponent
                  with ProfilesComponent
                  with LevelDbDatabaseComponent
 
        Log.info("[app] initializing")
        app.init()

        Log.info("==============================================")
        Log.info("Press enter to exit")
        Log.info("==============================================")
        readLine()

        Log.info("[app] shutting down")
        app.shutdown()
    }
}
