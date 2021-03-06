package tvon.server

import java.util.Date
import java.nio.file._
import Extensions._

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

case class ApiVideoLite (
  videoId          : String,
  title            : String,    
  season           : Option[Int],
  episodes         : List[Int],
  available        : Boolean,
  dateAdded        : Date,
  episodeTitle     : Option[String],
  year             : Option[Int],
  part             : Option[Int],
  rating           : Option[Double],
  runtime          : Option[String],
  actors           : List[String],
  writers          : List[String],
  directors        : List[String],
  image            : Option[String]
)

case class ApiVideo (
  videoId          : String,
  title            : String,    
  season           : Option[Int],
  episodes         : List[Int],
  available        : Boolean,
  dateAdded        : Date,
  episodeTitle     : Option[String],
  year             : Option[Int],
  part             : Option[Int],
  rating           : Option[Double],
  runtime          : Option[String],
  actors           : List[String],
  directors        : List[String],
  writers          : List[String],
  plot             : Option[String],
  languages        : List[String],
  countries        : List[String],
  image            : Option[String],
  technical        : String
)

case class ApiVideoList (
  videos: List[ApiVideoLite]
)

case class VideoLink(
  videoId:  String,
  imdbId:   Option[String],
  season:   Option[Int],
  episodes: List[Int]   
)

trait VideoEnvironment {
  def isQueued(profile: Profile, video: Video): Boolean
  def getImageUrl(s:Option[String]): Option[String]
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

object VideoOrdering extends Ordering[Video] {
  def compare(x:Video, y:Video): Int = {
      if (x.sortKey < y.sortKey)                 return -1
      if (x.sortKey > y.sortKey)                 return 1

      if (!x.season.isEmpty && y.season.isEmpty) return -1
      if (!y.season.isEmpty && x.season.isEmpty) return 1
      if (!x.season.isEmpty && !y.season.isEmpty) {
        if (x.season.get < y.season.get) return -1
        if (x.season.get > y.season.get) return 1
      }

      if (!x.episodes.isEmpty && y.episodes.isEmpty) return -1
      if (!y.episodes.isEmpty && x.episodes.isEmpty) return 1
      if (!x.episodes.isEmpty && !y.episodes.isEmpty) {
        if (x.episodes.min < y.episodes.min) return -1
        if (x.episodes.min > y.episodes.min) return 1
      }
      return x.videoId compare y.videoId
  }
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
  var basic            : BasicMetadata        = MetadataUtils.extractBasicMetadata(path)
  var title            : String               = basic.title
  var imdb             : Option[IMDBMetadata] = env.loadIMDBMetadata(this)
  var sortKey          : String               = Utils.getSortKey(title)
  var image            : Option[String]       = None
  var episodeTitle     : Option[String]       = basic.episodeTitle
  var runtime          : Option[String]       = None
  var rating           : Option[Double]       = None
  var imdbId           : Option[String]       = None

  def link: VideoLink = VideoLink(videoId = videoId, imdbId = imdbId, season = season, episodes = episodes)

  def compute() {
    // load imdb data
    imdb match {
      case None       =>
        actors       = List[Actor]()
        writers      = List[Writer]()
        directors    = List[Director]()
        genres       = List[Genre]()
        languages    = List[Language]()
        countries    = List[Country]()
        plot         = None
        year         = basic.year
        title        = basic.title 
        episodeTitle = basic.episodeTitle
        image        = None
        runtime      = None
        rating       = None
        imdbId       = None
      case Some(imdb) =>
        actors       = imdb.actors.map(env.getActor(_))
        writers      = imdb.writers.map(env.getWriter(_))
        directors    = imdb.directors.map(env.getDirector(_))
        genres       = imdb.genres.map(env.getGenre(_))
        languages    = imdb.language.map(env.getLanguage(_))
        countries    = imdb.country.map(env.getCountry(_))
        plot         = if (imdb.plot.isEmpty) { imdb.plot } else { imdb.plot_simple }
        year         = if (imdb.year.isEmpty) { basic.year } else { imdb.year }
        title        = imdb.title
        image        = imdb.poster
        runtime      = if (imdb.runtime.isEmpty) { None } else { Some(imdb.runtime.head) }
        rating       = imdb.rating
        imdbId       = Some(imdb.imdb_id)
        if (!season.isEmpty && !episodes.isEmpty && !imdb.episodes.isEmpty) {
          episodeTitle = imdb.episodes.get.tryPick(x => if (x.season == season.get && episodes.contains(x.episode) && !x.title.isEmpty) {
                                                          x.title
                                                        } else {
                                                          basic.episodeTitle
                                                        }) match {
                                                          case None => basic.episodeTitle
                                                          case some => some
                                                        }
        }
          basic.episodeTitle

    }

    if (isTv) {
      show = Some(env.getShow(title))
    } else {
      show = None
    }
    sortKey = Utils.getSortKey(title)
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
    updateBasicMetadata(MetadataUtils.extractBasicMetadata(path))
  }

  def matchesShow    (name: String): Boolean = !show.isEmpty && show.get == env.getShow(name)
  def matchesDirector(name: String): Boolean = directors.contains(env.getDirector(name))
  def matchesGenre   (name: String): Boolean = genres.contains(env.getGenre(name))
  def matchesWriter  (name: String): Boolean = writers.contains(env.getWriter(name))
  def matchesActor   (name: String): Boolean = actors.contains(env.getActor(name))
  def matchesLanguage(name: String): Boolean = languages.contains(env.getLanguage(name))
  def matchesCountry (name: String): Boolean = countries.contains(env.getCountry(name))
  def matchesLink    (link: VideoLink): Boolean = {
    (link.videoId == videoId) || 
    (!imdbId.isEmpty && imdbId == link.imdbId && season == link.season && episodes == link.episodes)
  }

  def isWatched (profile: Profile): Boolean = profile.history.exists(item => matchesLink(item.videoLink))
  def isQueued  (profile: Profile): Boolean = env.isQueued(profile, this)

  def watchedSince(profile: Profile, date: Date) = {
    profile.history.exists(item => matchesLink(item.videoLink) && item.watched.after(date))
  }

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

  def toApiLite: ApiVideoLite = {
    ApiVideoLite(
      videoId          = videoId,
      title            = title,
      season           = season,           
      episodes         = episodes,          
      available        = available,
      dateAdded        = dateAdded,
      episodeTitle     = episodeTitle,
      part             = part,
      year             = year,
      rating           = rating,
      runtime          = runtime,
      actors           = actors.take(3).map(_.name),
      writers          = writers.take(3).map(_.name),
      directors        = directors.take(3).map(_.name),
      image            = env.getImageUrl(image)
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
      episodeTitle     = episodeTitle,
      part             = part,
      year             = year,
      rating           = rating,
      runtime          = runtime,
      actors           = actors.map(_.name),
      writers          = writers.map(_.name),
      directors        = directors.map(_.name),
      plot             = plot,
      languages        = languages.map(_.name),
      countries        = countries.map(_.name),
      image            = env.getImageUrl(image),
      technical        = path.toString
    )
  }
}
