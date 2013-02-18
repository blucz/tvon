package tvon.server

import scala.collection.mutable._
import java.util.Date
import java.nio.file._

//
// Instances of this class should only be accessed from a single thread at a time
// 
// XXX: handle file moves
//
class Collection(val manager: Manager) {
  val videos                  = new HashMap[String,VideoFile]
  val onlineStorageBackendIds = new HashSet[String]

  def load() {
    for (json <- manager.db.loadVideoFiles()) {
      videos(json.videoId) = new VideoFile(this, json)
    }
    Log.info(s"[collection] loaded ${videos.size} existing files")
  }

  def makeDatabaseVideoFile(backend: StorageBackend, file: StorageFile): DatabaseVideoFile = {
    DatabaseVideoFile(
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

  def save(file: VideoFile) {
    manager.db.putVideoFile(file.toDatabase)
  }

  def tryFindVideo(storageBackendId: String, storageKey: String): Option[VideoFile] = {
    return videos.values.find(video => video.storageBackendId == storageBackendId && video.storageKey == storageKey)
  }

  //
  // Storage Integration
  //
  def getExistingFiles(backend: StorageBackend): List[ExistingFile] = {
    return videos.values.filter(v => v.storageBackendId == backend.id && !v.deleted).map(video => new ExistingFile(video.storageKey, video.modTime)).toList
  }

  def notifyOnline(backend: StorageBackend)  { onlineStorageBackendIds.add(backend.id)    }
  def notifyOffline(backend: StorageBackend) { onlineStorageBackendIds.remove(backend.id) }

  def notifyFilesAdded(backend: StorageBackend, files: List[StorageFile]) {
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
              var video = new VideoFile(this, makeDatabaseVideoFile(backend, file))
                                save(video)
                                videos(video.videoId) = video
          }
      }
    }
  }

  def notifyFilesRemoved(backend: StorageBackend, files: List[StorageFile]) {
    for (file <- files) {
      Log.trace(s"[collection] rm file ${file.path}")
      tryFindVideo(backend.id, file.key) match {
        case None        => Log.warning(s"[collection] missing file ${file.key}")
        case Some(video) => video.deleted = true
                            save(video)
      }
    }
  }

  def notifyFilesModified(backend: StorageBackend, files: List[StorageFile]) {
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

case class DatabaseVideoFile (
  videoId          : String,
  storageBackendId : String,
  storageKey       : String,
  modTime          : Long,
  deleted          : Boolean,
  dateAdded        : Date,
  signature        : Option[String],
  path             : String
)

case class ApiVideoFile (
  videoId          : String,
  title            : String,    
  season           : Option[Int],
  episodes         : List[Int],
  available        : Boolean,
  dateAdded        : Date,
  year             : Option[Int],
  part             : Option[Int]
)

case class ApiVideoList (
  videos: List[ApiVideoFile]
)

class VideoFile(collection: Collection, json: DatabaseVideoFile) {
  val videoId          : String         = json.videoId          
  val dateAdded        : Date           = json.dateAdded

  // mutable stuff that can change based on storage
  var storageBackendId : String         = json.storageBackendId 
  var storageKey       : String         = json.storageKey       
  var modTime          : Long           = json.modTime          
  var deleted          : Boolean        = json.deleted
  var signature        : Option[String] = json.signature
  var path             : Path           = Paths.get(json.path)

  // extract basic metadata from pathname
  var basic = MetadataUtils.extractBasicMetadata(path.getFileName.toString)

  def title            : String         = basic.title
  def season           : Option[Int]    = basic.season           
  def episodes         : List[Int]      = basic.episodes         
  def year             : Option[Int]    = basic.year
  def part             : Option[Int]    = basic.part

  def available        : Boolean        = collection.onlineStorageBackendIds.contains(storageBackendId) && !deleted
  def isTv             : Boolean        = !season.isEmpty 
  def isMovie          : Boolean        = !isTv
  def isOther          : Boolean        = false

  def updatePath(newpath:Path) {
    path   = newpath
    basic = MetadataUtils.extractBasicMetadata(path.getFileName().toString())
  }

  def matchesShow(show: String): Boolean = isTv && Utils.normalizedEquals(title, show)
  def matchesDirector(director: String): Boolean = false  // XXX: implement
  def matchesGenre(genre: String): Boolean = false  // XXX: implement
  def matchesActor(actor: String): Boolean = false  // XXX: implement
  def matchesCountry(country: String): Boolean = false  // XXX: implement

  def matchesSeason(testseason: Int): Boolean = {
    season match {
      case None           => false
      case Some(myseason) => myseason == testseason 
    }
  }

  def matchesDecade(testyear: Int): Boolean = {
    year match {
      case None         => false
      case Some(myyear) => myyear / 10 == testyear / 10
    }
  }

  def decade: Option[Int] = {
    year.map(_/10)
  }

  def toDatabase: DatabaseVideoFile = {
    DatabaseVideoFile(
      videoId          = videoId,
      storageBackendId = storageBackendId,
      storageKey       = storageKey,      
      modTime          = modTime,
      deleted          = deleted,
      dateAdded        = dateAdded,
      signature        = signature,
      path             = path.toString
    )
  }

  def toApi: ApiVideoFile = {
    ApiVideoFile(
      videoId          = videoId,
      title            = title,
      season           = season,           
      episodes         = episodes,          
      available        = available,
      dateAdded        = dateAdded,
      part             = part,
      year             = year
    )
  }
}
