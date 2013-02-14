package tvon.server

import scala.actors.Actor
import scala.actors.Actor._
import java.nio.file._

class Manager(config: Config) extends Actor {
    var backends   = List[StorageBackend]()
    val db         = new Database(Paths.get(config.datapath).resolve("tvon.db").toString())
    val collection = new Collection(this)
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
    private def ev_filesadded   (backend: StorageBackend, files: List[StorageFile]) { collection.notifyFilesAdded(backend, files)    }
    private def ev_filesremoved (backend: StorageBackend, files: List[StorageFile]) { collection.notifyFilesRemoved(backend, files)  }
    private def ev_filesmodified(backend: StorageBackend, files: List[StorageFile]) { collection.notifyFilesModified(backend, files) }
    private def ev_online (backend: StorageBackend) { collection.notifyOnline(backend)  }
    private def ev_offline(backend: StorageBackend) { collection.notifyOffline(backend) }

    private def ev_shutdown() { exit() }

    private def ev_init() {
        collection.load()
        for (dirconfig <- config.directories) {
          val backend = new DirectoryStorageBackend(dirconfig, extensions)
          backends = backend :: backends
          backend.watch(collection.getExistingFiles(backend), this)
        }
    }

    //
    // Actor dispatch routine
    //
    def act() {
      loop { react {
        case StorageBackend.FilesAdded(backend, files)    => ev_filesadded(backend, files)
        case StorageBackend.FilesModified(backend, files) => ev_filesmodified(backend, files)
        case StorageBackend.FilesRemoved(backend, files)  => ev_filesremoved(backend, files)
        case StorageBackend.Online(backend)               => ev_online(backend)
        case StorageBackend.Offline(backend)              => ev_offline(backend)
        case Manager.Post(action)                         => action()
        case Manager.Init                                 => ev_init()
        case Manager.Shutdown                             => ev_shutdown()
      } }
    }
}

object Manager {
    case object Init
    case object Shutdown
    case class  Post(action: Unit => Unit)
}
