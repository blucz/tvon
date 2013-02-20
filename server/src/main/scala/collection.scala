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

trait CollectionComponent extends Lifecycle with Lock { 
  this: CollectionDatabaseComponent with MetadataLookupComponent with BrowserComponent =>
  val collection: Collection = new Collection

  override def init() {
    Log.info("[collection] initializing")
    collection.init()
    super.init()
  }

  //
  // All public methods are synchronized for thread-safety
  //
  class Collection {
    private val videos                  = new HashMap[String,Video]
    private val onlineStorageBackendIds = new HashSet[String]

    def init() {
      for (json <- db.loadVideos()) {
        videos(json.videoId) = new Video(env, json)
      }
      Log.info(s"[collection] loaded ${videos.size} existing files")
    }

    // public API
    def getVideo(videoId: String): Option[Video] = lock { videos.get(videoId) }
    def allVideos: List[Video]                   = lock { videos.values.toList }
    def availableVideos: List[Video]             = lock { videos.values.filter(_.available).toList }

    private val countries = new HashMap[String,Country]()
    private val directors = new HashMap[String,Director]()
    private val actors    = new HashMap[String,Actor]()
    private val writers   = new HashMap[String,Writer]()
    private val languages = new HashMap[String,Language]()
    private val genres    = new HashMap[String,Genre]()
    private val shows     = new HashMap[String,Show]()

    private def getImmutable[A](map: HashMap[String,A], name: String, cons: (String) => A): A = {
      val key = Utils.getMergeKey(name)
      map.get(key) match {
        case None       => val item = cons(name)
                           map(key) = item
                           item
        case Some(item) => item
      }
    }

    private val env = new VideoEnvironment {
      def isBackendOnline(storageBackendId: String): Boolean = lock {
        onlineStorageBackendIds.contains(storageBackendId)
      }

      def getCountry (name: String): Country  = lock { getImmutable(countries, name, new Country(_))  }
      def getDirector(name: String): Director = lock { getImmutable(directors, name, new Director(_)) }
      def getActor   (name: String): Actor    = lock { getImmutable(actors,    name, new Actor(_))    }
      def getWriter  (name: String): Writer   = lock { getImmutable(writers,   name, new Writer(_))   }
      def getLanguage(name: String): Language = lock { getImmutable(languages, name, new Language(_)) }
      def getGenre   (name: String): Genre    = lock { getImmutable(genres,    name, new Genre(_))    }
      def getShow    (name: String): Show     = lock { getImmutable(shows,     name, new Show(_))     }
      def getImageUrl(url:  Option[String]): Option[String] = browser.getImageUrl(url)

      def loadIMDBMetadata(video: Video): Option[IMDBMetadata] = lock {
        metadatalookup.loadIMDBMetadata(video)
      }
    }

    def updateIMDBMetadata(video: Video, imdb: Option[IMDBMetadata]) { lock {
      video.updateIMDBMetadata(imdb)
    } }

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
      val existing = lock {
        videos.values.filter(v => v.storageBackendId == backend.id && !v.deleted).map(video => new ExistingFile(video.storageKey, video.modTime)).toList
      }
      backend.watch(existing, (ev:StorageBackend.Event) => lock {
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
                var video = new Video(env, makeDatabaseVideo(backend, file))
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
