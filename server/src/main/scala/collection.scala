package tvon.server

import scala.collection.mutable._
import java.util.UUID
import java.util.Date

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

  def makeVideoFileJSON(backend: StorageBackend, file: StorageFile): VideoFileJSON = {
    val basic = MetadataUtils.extractBasicMetadata(file.path.getFileName().toString())
    new VideoFileJSON(
      videoId          = UUID.randomUUID.toString,
      storageBackendId = backend.id,
      storageKey       = file.key,
      modTime          = file.modTime,
      title            = basic.title,
      season           = basic.season, 
      episodes         = basic.episodes,
      dateAdded        = new Date(),
      deleted          = false,
      signature        = file.signature
    )
  }

  def save(file: VideoFile) {
    manager.db.putVideoFile(file.toJSON)
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
                                save(video)
            case None =>
              Log.trace(s"[collection] add new file ${file.path}")
              var video = new VideoFile(this, makeVideoFileJSON(backend, file))
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
        case None            => Log.warning(s"[collection] missing file ${file.key}")
        case Some(videofile) => videofile.deleted = true
                                save(videofile)
      }
    }
  }

  def notifyFilesModified(backend: StorageBackend, files: List[StorageFile]) {
    for (file <- files) {
      Log.trace(s"[collection] mod file ${file.path}")
      tryFindVideo(backend.id, file.key) match {
        case None            => Log.warning(s"[collection] missing file ${file.key}")
        case Some(videofile) => videofile.modTime = file.modTime
                                videofile.deleted = false
                                save(videofile)
      }
    }
  }
}

case class VideoFileJSON (
  videoId          : String,
  storageBackendId : String,
  storageKey       : String,
  modTime          : Long,
  title            : String,    
  season           : Option[Int],
  episodes         : List[Int],
  deleted          : Boolean,
  dateAdded        : Date,
  signature        : Option[String]
)

class VideoFile(collection: Collection, json: VideoFileJSON) {
  val videoId          : String         = json.videoId          
  var storageBackendId : String         = json.storageBackendId 
  var storageKey       : String         = json.storageKey       
  var modTime          : Long           = json.modTime          
  var title            : String         = json.title
  var season           : Option[Int]    = json.season           
  var episodes         : List[Int]      = json.episodes         
  var deleted          : Boolean        = json.deleted
  var dateAdded        : Date           = json.dateAdded
  var signature        : Option[String] = json.signature
  def isAvailable      : Boolean        = collection.onlineStorageBackendIds.contains(storageBackendId) && !deleted
  //def metadata         : Option[IMDBMetadataJSON] = resolveMetadata

  def toJSON: VideoFileJSON = {
    new VideoFileJSON(
      videoId          = videoId,
      storageBackendId = storageBackendId,
      storageKey       = storageKey,      
      modTime          = modTime,
      title            = title,
      season           = season,           
      episodes         = episodes,          
      deleted          = deleted,
      dateAdded        = dateAdded,
      signature        = signature
    )
  }
}
