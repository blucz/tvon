package tvon.server

import scala.collection.mutable._
import java.util.Date
import java.nio.file._

trait CollectionDatabaseComponent { val db: CollectionDatabase }
trait CollectionDatabase extends Database {
    def tryGetVideo(videoId: String): Option[DatabaseVideo]
    def putVideo(videoFile: DatabaseVideo)
    def loadVideos(): List[DatabaseVideo]
    def deleteVideo(videoId: String)
}

trait CollectionComponent extends Lifecycle { this: CollectionDatabaseComponent =>
  val collection: Collection = new Collection
  override def init() {
    Log.info("[collection] initializing")
    collection.init()
    super.init()
  }
  class Collection extends VideoEnvironment {
    val videos                  = new HashMap[String,Video]
    private val onlineStorageBackendIds = new HashSet[String]

    def init() {
      for (json <- db.loadVideos()) {
        videos(json.videoId) = new Video(this, json)
      }
      Log.info(s"[collection] loaded ${videos.size} existing files")
    }

    def isBackendOnline(storageBackendId: String): Boolean = {
      onlineStorageBackendIds.contains(storageBackendId)
    }

    private def makeDatabaseVideo(backend: StorageBackend, file: StorageFile): DatabaseVideo = {
      DatabaseVideo(
        videoId          = Utils.newGuid,
        path             = file.path.toString(),
        storageBackendId = backend.id,
        storageKey       = file.key,
        modTime          = file.modTime,
        dateAdded        = new Date(),
        deleted          = false,
        signature        = file.signature
      )
    }

    private def save(file: Video) {
      db.putVideo(file.toDatabase)
    }

    private def tryFindVideo(storageBackendId: String, storageKey: String): Option[Video] = {
      return videos.values.find(video => video.storageBackendId == storageBackendId && video.storageKey == storageKey)
    }

    //
    // Storage Integration
    //
    def loadBackend(backend: StorageBackend) {
        val existing = videos.values.filter(v => v.storageBackendId == backend.id && !v.deleted).map(video => new ExistingFile(video.storageKey, video.modTime)).toList
        backend.watch(existing, (ev:StorageBackend.Event) => {
          ev match {
            case StorageBackend.FilesAdded(backend, files)    => onFilesAdded(backend, files)
            case StorageBackend.FilesModified(backend, files) => onFilesModified(backend, files)
            case StorageBackend.FilesRemoved(backend, files)  => onFilesRemoved(backend, files)
            case StorageBackend.Online(backend)               => onBackendOnline(backend)
            case StorageBackend.Offline(backend)              => onBackendOffline(backend)
          }
        })
    }

    private def onBackendOnline (backend: StorageBackend) { onlineStorageBackendIds.add(backend.id)    }
    private def onBackendOffline(backend: StorageBackend) { onlineStorageBackendIds.remove(backend.id) }

    private def onFilesAdded(backend: StorageBackend, files: List[StorageFile]) {
      for (file <- files) {
        tryFindVideo(backend.id, file.key) match {
          case Some(video) => if (video.deleted) {
                                Log.trace(s"[collection] file undeleted ${file.key}")
                                video.deleted = false
                                save(video)
                              } else {
                                Log.warning(s"[collection] ignoring duplicate file ${file.key}")
                              }
          case None        => 
            videos.values.find(v => v.signature == file.signature && v.deleted) match {
              case Some(video) => Log.trace(s"[collection] file moved from ${video.storageKey} => ${file.key}")
                                  video.storageBackendId = backend.id
                                  video.storageKey       = file.key
                                  video.deleted          = false
                                  video.updatePath(file.path)
                                  save(video)
              case None =>
                Log.trace(s"[collection] add new file ${file.path}")
                var video = new Video(this, makeDatabaseVideo(backend, file))
                                  save(video)
                                  videos(video.videoId) = video
            }
        }
      }
    }

    private def onFilesRemoved(backend: StorageBackend, files: List[StorageFile]) {
      for (file <- files) {
        Log.trace(s"[collection] rm file ${file.path}")
        tryFindVideo(backend.id, file.key) match {
          case None        => Log.warning(s"[collection] missing file ${file.key}")
          case Some(video) => video.deleted = true
                              save(video)
        }
      }
    }

    private def onFilesModified(backend: StorageBackend, files: List[StorageFile]) {
      for (file <- files) {
        Log.trace(s"[collection] mod file ${file.path}")
        tryFindVideo(backend.id, file.key) match {
          case None        => Log.warning(s"[collection] missing file ${file.key}")
          case Some(video) => video.modTime = file.modTime
                                  video.deleted = false
                                  video.updatePath(file.path)
                                  save(video)
        }
      }
    }
  }
}
