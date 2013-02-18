package tvon.server

object App extends {
    def main(args: Array[String]) {
        org.slf4j.LoggerFactory.getLogger("ROOT")
        val appconfig = Config.load("config.json") match {
          case Left(config) => config 
          case Right(err)   => println(s"Error loading configuration: ${err}")
                               System.exit(1)
                               return
        }

        class ConfigImpl extends ConfigComponent { val config = appconfig }
        class AppInstance extends ConfigImpl
                             with LevelDbDatabaseComponent
                             with ProfilesComponent
                             with CollectionComponent
                             with BrowserComponent
                             with WebServerComponent
        val app = new AppInstance

        Log.info("initializing")
        app.profiles.init()
        app.collection.init()
        app.webServer.start()

        var backends = List[StorageBackend]()

        Log.info("loading storage backends")
        for (dirconfig <- appconfig.directories) {
          val backend = new DirectoryStorageBackend(dirconfig, appconfig.extensions)
          backends = backend::backends
          app.collection.loadBackend(backend)
        }

        Log.info("==============================================")
        Log.info("Press enter to exit")
        Log.info("==============================================")
        readLine()

        Log.info("stopping storage")
        for (backend <- backends) {
          backend.close()
        }
        Log.info("exiting")
        app.db.close()
        Log.info("stopping server")
        app.webServer.stop()
    }
}
