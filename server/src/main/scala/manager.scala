package tvon.server

import scala.actors.Actor
import scala.actors.Actor._

class Database {
}

object Manager {
    case object Init
    case object Shutdown
    case class  Post(action: Unit => Unit)
}

class Manager(config: Config) extends Actor {
    var backends   = List[StorageBackend]()
    val db         = new Database()
    val extensions = List(".avi", ".mkv", ".mp4")

    //
    // Public API, threadsafe
    //
    def initialize()               { start() ! Manager.Init          }
    def shutdown()                 { this    ! Manager.Shutdown      }
    def post(action: Unit => Unit) { this    ! Manager.Post(action)  }

    //
    // Private implementations (properly serialized)
    //
    private def ev_filesadded(files: List[StorageFile]) {
        for (file <- files) {
          Log.info(s"[manager] add file ${file.path}")
        }
    }

    private def ev_filesremoved(files: List[StorageFile]) {
        for (file <- files) {
          Log.info(s"[manager] rm file ${file.path}")
        }
    }

    private def ev_filesmodified(files: List[StorageFile]) {
        for (file <- files) {
          Log.info(s"[manager] mod file ${file.path}")
        }
    }

    private def ev_init() {
        Log.info("initializing manager")
        for (dirconfig <- config.directories) {
          val backend = new DirectoryStorageBackend(dirconfig, extensions)
          backends = backend :: backends
          backend.watch(List[ExistingFile](), this)
        }
    }

    private def ev_shutdown() {
      exit()
    }

    //
    // Actor dispatch routine
    //
    def act() {
      loop { react {
        case StorageBackend.FilesAdded(files)    => ev_filesadded(files)
        case StorageBackend.FilesModified(files) => ev_filesmodified(files)
        case StorageBackend.FilesRemoved(files)  => ev_filesremoved(files)
        case Manager.Post(action)                => action()
        case Manager.Init                        => ev_init()
        case Manager.Shutdown                    => ev_shutdown()
      } }
    }
}

