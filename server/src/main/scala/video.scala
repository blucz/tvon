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
  def getCountry(name: String): Country 
  def getDirector(name: String): Director 
  def getShow(name: String): Show 
  def getLanguage(name: String): Language 
  def getWriter(name: String): Writer 
  def getActor(name: String): Actor 
  def getGenre(name: String): Genre 
  def loadIMDBMetadata(video: Video): Option[IMDBMetadata]
}

class Language(val name: String) {
  val sortKey  = Utils.getSortKey(name)
  val mergeKey = Utils.getMergeKey(name)
}

class Country(val name: String) {
  val sortKey  = Utils.getSortKey(name)
  val mergeKey = Utils.getMergeKey(name)
}

class Show(val name: String) {
  val sortKey  = Utils.getSortKey(name)
  val mergeKey = Utils.getMergeKey(name)
}

class Writer(val name: String) {
  val sortKey  = Utils.getSortKey(name)
  val mergeKey = Utils.getMergeKey(name)
}

class Actor(val name: String) {
  val sortKey  = Utils.getSortKey(name)
  val mergeKey = Utils.getMergeKey(name)
}

class Director(val name: String) {
  val sortKey  = Utils.getSortKey(name)
  val mergeKey = Utils.getMergeKey(name)
}

class Genre(val name: String) {
  val sortKey  = Utils.getSortKey(name)
  val mergeKey = Utils.getMergeKey(name)
}

class Video(env: VideoEnvironment, json: DatabaseVideo) {
  val videoId          : String               = json.videoId          
  val dateAdded        : Date                 = json.dateAdded
  var storageBackendId : String               = json.storageBackendId 
  var storageKey       : String               = json.storageKey       
  var modTime          : Long                 = json.modTime          
  var deleted          : Boolean              = json.deleted
  var signature        : Option[String]       = json.signature
  var path             : Path                 = Paths.get(json.path)
  var actors           : List[Actor]          = List[Actor]()
  var writers          : List[Writer]         = List[Writer]()
  var directors        : List[Director]       = List[Director]()
  var genres           : List[Genre]          = List[Genre]()
  var languages        : List[Language]       = List[Language]()
  var countries        : List[Country]        = List[Country]()
  var plot             : Option[String]       = None
  var show             : Option[Show]         = None
  var year             : Option[Int]          = None
  var basic            : BasicMetadata        = MetadataUtils.extractBasicMetadata(path.getFileName.toString)
  var title            : String               = basic.title
  var imdb             : Option[IMDBMetadata] = env.loadIMDBMetadata(this)

  def compute() {
    // load imdb data
    imdb match {
      case None       =>
        actors     = List[Actor]()
        writers    = List[Writer]()
        directors  = List[Director]()
        genres     = List[Genre]()
        languages  = List[Language]()
        countries  = List[Country]()
        plot       = None
        year       = basic.year
        title      = basic.title 
      case Some(imdb) =>
        actors     = imdb.actors.map(env.getActor(_))
        writers    = imdb.writers.map(env.getWriter(_))
        directors  = imdb.directors.map(env.getDirector(_))
        genres     = imdb.genres.map(env.getGenre(_))
        languages  = imdb.language.map(env.getLanguage(_))
        countries  = imdb.country.map(env.getCountry(_))
        plot       = if (imdb.plot.isEmpty) { imdb.plot } else { imdb.plot_simple }
        year       = if (imdb.year.isEmpty) { basic.year } else { imdb.year }
        title      = imdb.title
    }

    //dump
    if (isTv) {
      show = Some(env.getShow(title))
    } else {
      show = None
    }
  }

  def dump {
    Log.info(s"======================================================")
    Log.info(s"Video title:        $title")
    Log.info(s"      path:         $path")
    if (isTv) {
      Log.info(s"      season:       $season")
      Log.info(s"      episodes:     $episodes")
    }
    Log.info(s"      dateAdded:    $dateAdded")
    Log.info(s"      actors:       $actors")
    Log.info(s"      directors:    $directors")
    Log.info(s"      writers:      $writers")
    Log.info(s"      genres:       $genres")
    Log.info(s"      languages:    $languages")
    Log.info(s"      countries:    $countries")
    Log.info(s"      plot:         $plot")
    Log.info(s"======================================================")
  }

  compute()

  def part             : Option[Int]    = basic.part
  def season           : Option[Int]    = basic.season           
  def episodes         : List[Int]      = basic.episodes         

  def available        : Boolean        = env.isBackendOnline(storageBackendId) && !deleted
  def isTv             : Boolean        = !season.isEmpty 
  def isMovie          : Boolean        = !isTv
  def isOther          : Boolean        = false

  def updateBasicMetadata(basic: BasicMetadata) {
    this.basic = basic
    compute()
  }

  def updateIMDBMetadata(imdb: Option[IMDBMetadata]) {
    this.imdb = imdb
    compute()
  }

  def updatePath(newpath:Path) {
    path   = newpath
    updateBasicMetadata(MetadataUtils.extractBasicMetadata(path.getFileName().toString()))
  }

  def matchesShow    (name: String): Boolean = !show.isEmpty && show.get == env.getShow(name)
  def matchesDirector(name: String): Boolean = directors.contains(env.getDirector(name))
  def matchesGenre   (name: String): Boolean = genres.contains(env.getGenre(name))
  def matchesWriter  (name: String): Boolean = writers.contains(env.getWriter(name))
  def matchesActor   (name: String): Boolean = actors.contains(env.getActor(name))
  def matchesLanguage(name: String): Boolean = languages.contains(env.getLanguage(name))
  def matchesCountry (name: String): Boolean = countries.contains(env.getCountry(name))

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
