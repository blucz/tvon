package tvon.server

import java.util.Date
import java.nio.file._

case class DatabaseVideo (
  videoId          : String,
  storageBackendId : String,
  storageKey       : String,
  modTime          : Long,
  deleted          : Boolean,
  dateAdded        : Date,
  signature        : Option[String],
  path             : String
)

case class ApiVideo (
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
  videos: List[ApiVideo]
)

trait VideoEnvironment {
  def isBackendOnline(storageBackendId: String): Boolean
}

class Video(env: VideoEnvironment, json: DatabaseVideo) {
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

  def available        : Boolean        = env.isBackendOnline(storageBackendId) && !deleted
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
    year.map(_/10*10)
  }

  def toDatabase: DatabaseVideo = {
    DatabaseVideo(
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

  def toApi: ApiVideo = {
    ApiVideo(
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
