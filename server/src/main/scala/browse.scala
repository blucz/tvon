package tvon.server

import java.net.URLEncoder
import Extensions._
import scala.collection.mutable._

trait BrowseKeyDatabaseComponent  { val db: BrowseKeyDatabase  }
trait BrowseKeyDatabase extends Database {
    def keyToString(k:String): Option[String]
    def stringToKey(s:String): String
}

case class ApiBrowseAction(name: String, action: String)
case class ApiBrowseMetadata(label: String, value: String)

case class ApiBrowseItem(
  path:          String,
  title:         String,
  subtitles:     List[String]             = List[String](),
  metadata:      List[ApiBrowseMetadata]  = List[ApiBrowseMetadata](),
  technical:     String                   = "",
  canHaveImage:  Boolean                  = false,
  image:         Option[String]           = None,
  actions:       List[ApiBrowseAction]    = List[ApiBrowseAction]()
)

case class ApiBrowseLevel(
  items: List[ApiBrowseItem] = List[ApiBrowseItem](),
  video: Option[ApiVideo]    = None
)

case class BrowseParameters(
  path:      String,
  screenId:  Option[String],
  profileId: Option[String]
) {
  def toQueryParameters: QueryParameters = QueryParameters(screenId = screenId, profileId = profileId)
}

//
// browse paths
//
//     /                      = root
//     /queue/movies/...      = root
//     /movies/decades        = list of decade levels
//     /movies/decades/1990s  =

trait BrowserComponent { 
  this: CollectionComponent with ProfilesComponent with QueryComponent with BrowseKeyDatabaseComponent =>
  val browser = new Browser

  class Browser {
    private def browse_root(params: BrowseParameters): Option[ApiBrowseLevel] = {
      val items = List[ApiBrowseItem](
        ApiBrowseItem(path = "/queue",            title = s"My Queue"),
        ApiBrowseItem(path = "/movies/filters",   title = s"Movies"),
        ApiBrowseItem(path = "/tv/shows",         title = s"TV"),    
        ApiBrowseItem(path = "/other",            title = s"Other")
      )
      finishBrowse(items)
    }

    private def browse_filters(params: BrowseParameters, videos: List[Video]): Option[ApiBrowseLevel] = {
      val items = List[ApiBrowseItem](
        ApiBrowseItem(path = "/movies/recent/30",     title = s"Recently Added"),
        ApiBrowseItem(path = "/movies",               title = s"All"),
        ApiBrowseItem(path = "/movies/decades",       title = s"Decades"),    
        //ApiBrowseItem(path = "/movies/actors",        title = s"Actors"),           disabled because slow in browser..need alphanav
        //ApiBrowseItem(path = "/movies/directors",     title = s"Directors"),
        ApiBrowseItem(path = "/movies/genres",        title = s"Genres"),
        ApiBrowseItem(path = "/movies/countries",     title = s"Countries")
      )
      finishBrowse(items)
    }

    // generate a browse level within a path
    def browse(params: BrowseParameters): Option[ApiBrowseLevel] = {
      Log.debug(s"[browser] browse ${params.path}")
      val comps = params.path.split('/').filter(_ != "").toList
      if (comps.length == 0) {
        browse_root(params)
      } else {
        browse(params, comps, collection.allVideos.toList)
      }
    }

    private def join(a:String, b:String) = a + "/" + b

    private def videoSubtitle(video: Video): String = {
      if (video.isTv) {
        val e = video.episodes match {
          case Nil         => "?"
          case e1::e2::Nil => s"${e1}-${e2}"
          case es          => es mkString ", "
        }
        val s = video.season getOrElse "?"
        s"Season ${s}, Episode ${e}"
      } else {
        yearRuntimeString(video) getOrElse ""
      }
    }

    def yearRuntimeString(video: Video): Option[String] = {
        (video.year,video.runtime) match {
          case (None,None)                => None
          case (Some(year),None)          => Some(year.toString)
          case (None,Some(runtime))       => Some(runtime)
          case (Some(year),Some(runtime)) => Some(year.toString + ", " + runtime)
        }
    }

    def getImageUrl(url: Option[String]) = url.map("/images?url=" + URLEncoder.encode(_, "utf8"))

    private def browse_videos(params: BrowseParameters, videos: List[Video]): Option[ApiBrowseLevel] = {
      if (videos.length == 0) { return None }

      if (videos.forall(video => video.show == videos(0).show) && videos.map(_.season).distinct.length != 1) {
        // we have all the same show, but multiple seasons, so insert new browse level
        return browse_seasons(params, videos)
      }

      if (videos.length == 1) {
        return finishBrowse(videos.head.toApi)
      }

      val item     = ApiBrowseItem(path = params.path, title = s"${videos.length} Results")
      val sortedvideos = videos.sorted(VideoOrdering)

      val items = sortedvideos.map(video => ApiBrowseItem(
        path         = join("/videos", video.videoId),
        title        = video.episodeTitle getOrElse video.title,
        subtitles    = List(videoSubtitle(video)),
        canHaveImage = true,
        image        = getImageUrl(video.image),
        technical    = video.path.toString,
        metadata     = {
          var metadata = ArrayBuffer[ApiBrowseMetadata]()
          if (video.actors.length    > 0) metadata += ApiBrowseMetadata("Starring: ", video.actors.take(4).map(_.name) mkString ", ")
          if (video.directors.length > 0) metadata += ApiBrowseMetadata("Directed By: ", video.directors.take(4).map(_.name) mkString ", ")
          if (video.writers.length   > 0) metadata += ApiBrowseMetadata("Written By: ",  video.writers.take(4).map(_.name)   mkString ", ")
          metadata.toList
        }
      ))
      finishBrowse(items)
    }

    private def browse_decades(params: BrowseParameters, videos: List[Video]): Option[ApiBrowseLevel] = {
      val items = videos.filter(!_.decade.isEmpty).map(_.decade.get).filter(_ > 1880).sorted.distinct.map(decade =>
          ApiBrowseItem(path = join(params.path, decade.toString), title = s"${decade}s")
      )
      finishBrowse(items)
    }

    private def browse_genres(params: BrowseParameters, videos: List[Video]): Option[ApiBrowseLevel] = {
      val items = videos.flatMap(_.genres).distinct.sortBy(_.sortKey).map(genre => 
          ApiBrowseItem(path = join(params.path, db.stringToKey(genre.name)), title = genre.name)
      )
      finishBrowse(items)
    }

    private def browse_actors(params: BrowseParameters, videos: List[Video]): Option[ApiBrowseLevel] = {
      val items = videos.flatMap(_.actors).distinct.sortBy(_.sortKey).map(actor => 
          ApiBrowseItem(path = join(params.path, db.stringToKey(actor.name)), title = actor.name)
      )
      finishBrowse(items)
    }

    private def browse_directors(params: BrowseParameters, videos: List[Video]): Option[ApiBrowseLevel] = {
      val items = videos.flatMap(_.directors).distinct.sortBy(_.sortKey).map(director => 
          ApiBrowseItem(path = join(params.path, db.stringToKey(director.name)), title = director.name)
      )
      finishBrowse(items)
    }

    private def browse_countries(params: BrowseParameters, videos: List[Video]): Option[ApiBrowseLevel] = {
      val items = videos.flatMap(_.countries).distinct.sortBy(_.sortKey).map(country => 
          ApiBrowseItem(path = join(params.path, db.stringToKey(country.name)), title = country.name)
      )
      finishBrowse(items)
    }

    private def browse_seasons(params: BrowseParameters, videos: List[Video]): Option[ApiBrowseLevel] = {
      val items = videos.filter(!_.season.isEmpty).map(_.season.get).distinct.sorted.map(season =>
          ApiBrowseItem(path         = join(params.path, "seasons/" + season.toString), 
                        title        = s"Season ${season}",
                        canHaveImage = true,
                        image        = getImageUrl(videos(0).image),
                        metadata     = {
                          val video = videos(0)
                          var metadata = ArrayBuffer[ApiBrowseMetadata]()
                          if (video.actors.length    > 0) metadata += ApiBrowseMetadata("Starring: ", video.actors.take(4).map(_.name) mkString ", ")
                          if (video.directors.length > 0) metadata += ApiBrowseMetadata("Directed By: ", video.directors.take(4).map(_.name) mkString ", ")
                          if (video.writers.length   > 0) metadata += ApiBrowseMetadata("Written By: ",  video.writers.take(4).map(_.name)   mkString ", ")
                          metadata.toList
                        }
                       )
      )
      finishBrowse(items)
    }

    private def browse_shows(params: BrowseParameters, videos: List[Video]): Option[ApiBrowseLevel] = {
      val items = videos.filter(!_.show.isEmpty).distinctWith(_.show.get).sortBy(_.sortKey).map(video => 
          ApiBrowseItem(path         = join(params.path, db.stringToKey(video.title)), 
                        title        = video.show.get.name,
                        canHaveImage = true,
                        image        = getImageUrl(video.image),
                        subtitles    = yearRuntimeString(video).toList,
                        metadata     = {
                          var metadata = ArrayBuffer[ApiBrowseMetadata]()
                          if (video.actors.length    > 0) metadata += ApiBrowseMetadata("Starring: ", video.actors.take(4).map(_.name) mkString ", ")
                          if (video.directors.length > 0) metadata += ApiBrowseMetadata("Directed By: ", video.directors.take(4).map(_.name) mkString ", ")
                          if (video.writers.length   > 0) metadata += ApiBrowseMetadata("Written By: ",  video.writers.take(4).map(_.name)   mkString ", ")
                          metadata.toList
                        }
                       )
      )
      finishBrowse(items)
    }

    private def finishBrowse(video: ApiVideo): Option[ApiBrowseLevel] = Some(ApiBrowseLevel(video = Some(video)))

    private def finishBrowse(items: List[ApiBrowseItem]): Option[ApiBrowseLevel] = items match {
      case Nil => None
      case _   => Some(ApiBrowseLevel(items = items))
    }

    private def browse(params: BrowseParameters, fullpath: List[String], invideos: List[Video]): Option[ApiBrowseLevel] = {
      val (videos, path) = query.select(params.toQueryParameters, fullpath, invideos)
      path match {
        case Nil                => browse_videos(params, videos)             
        case "countries"::Nil   => browse_countries(params, videos)          
        case "decades"::Nil     => browse_decades(params, videos)            
        case "genres"::Nil      => browse_genres(params, videos)             
        case "actors"::Nil      => browse_actors(params, videos)            
        case "directors"::Nil   => browse_directors(params, videos)    
        case "seasons"::Nil     => browse_seasons(params, videos)
        case "shows"::Nil       => browse_shows(params, videos)
        case "filters"::Nil     => browse_filters(params, videos)
        case _                  => None
      }
    }
  }
}
