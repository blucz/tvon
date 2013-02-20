package tvon.server

import java.net.URLEncoder
import Extensions._

trait BrowseKeyDatabaseComponent  { val db: BrowseKeyDatabase  }
trait BrowseKeyDatabase extends Database {
    def keyToString(k:String): Option[String]
    def stringToKey(s:String): String
}

case class ApiBrowseAction(name: String, action: String)

case class ApiBrowseItem(
  path:          String,
  title:         String,
  subtitle:      String                   = "",
  technical:     String                   = "",
  canHaveImage:  Boolean                  = false,
  image:         Option[String]           = None,
  actions:       List[ApiBrowseAction]    = List[ApiBrowseAction](),
  video:         Option[ApiVideoLite]     = None
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
  def withPath(newpath: String): BrowseParameters = {
    BrowseParameters(newpath, screenId, profileId)
  }
}

//
// browse paths
//
//     /                      = root
//     /queue/movies/...      = root
//     /movies/decades        = list of decade levels
//     /movies/decades/1990s  =

trait BrowserComponent { 
  this: CollectionComponent with ProfilesComponent with BrowseKeyDatabaseComponent =>
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
        ApiBrowseItem(path = "/movies/recentlyadded", title = s"Recently Added"),
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

    // select a list of videos that fall within a path
    def select(params: BrowseParameters): (List[Video],List[String]) = {
      select(params, params.path.split('/').filter(_ != "").toList)
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
        val t = video.episodeTitle.map(" - " + _) getOrElse ""
        s"Season ${s}, Episode ${e}${t}"
      } else {
        video.year.map(_.toString) getOrElse ""
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
        path         = join("videos", video.videoId),
        title        = video.title,
        subtitle     = videoSubtitle(video),
        canHaveImage = true,
        image        = getImageUrl(video.image),
        technical    = video.path.toString,
        video        = Some(video.toApiLite)
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
                        image        = getImageUrl(videos(0).image))
      )
      finishBrowse(items)
    }

    private def browse_shows(params: BrowseParameters, videos: List[Video]): Option[ApiBrowseLevel] = {
      val items = videos.filter(!_.show.isEmpty).distinctWith(_.show.get).sortBy(_.sortKey).map(video => 
          ApiBrowseItem(path         = join(params.path, db.stringToKey(video.title)), 
                        title        = video.show.get.name,
                        canHaveImage = true,
                        image        = getImageUrl(video.image))
      )
      finishBrowse(items)
    }

    private def finishBrowse(video: ApiVideo): Option[ApiBrowseLevel] = Some(ApiBrowseLevel(video = Some(video)))

    private def finishBrowse(items: List[ApiBrowseItem]): Option[ApiBrowseLevel] = items match {
      case Nil => None
      case _   => Some(ApiBrowseLevel(items = items))
    }

    private def browse(params: BrowseParameters, fullpath: List[String], invideos: List[Video]): Option[ApiBrowseLevel] = {
      val (videos, path) = select(params, fullpath, invideos)
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

    private def count(params: BrowseParameters) {
      val (videos, path) = select(params)
      videos.length
    }

    private def select(params: BrowseParameters, path: List[String]): (List[Video],List[String]) = {
      select(params, path, collection.allVideos.toList)
    }

    private def select(params: BrowseParameters, path: List[String], videos: List[Video]): (List[Video],List[String]) = {
      object ParseInt {
        def unapply(s:String) : Option[Int] = {
          try   { Some(s.toInt)                        }   
          catch { case e:NumberFormatException => None }
        }
      }

      object ParseKey {
        def unapply(s:String) : Option[String] = db.keyToString(s)
      }

      path match {
        case Nil                                  => (videos,List[String]())
        case "movies"::tl                         => select(params, tl, videos.filter(_.isMovie))
        case "tv"::tl                             => select(params, tl, videos.filter(_.isTv))
        case "other"::tl                          => select(params, tl, videos.filter(_.isOther))
        case "decades"::ParseInt(decade)::tl      => select(params, tl, videos.filter(_.matchesDecade(decade)))
        case "seasons"::ParseInt(season)::tl      => select(params, tl, videos.filter(_.matchesSeason(season)))
        case "shows"::ParseKey(show)::tl          => select(params, tl, videos.filter(_.matchesShow(show)))
        case "directors"::ParseKey(director)::tl  => select(params, tl, videos.filter(_.matchesDirector(director)))
        case "actors"::ParseKey(actor)::tl        => select(params, tl, videos.filter(_.matchesActor(actor)))
        case "genres"::ParseKey(genre)::tl        => select(params, tl, videos.filter(_.matchesGenre(genre)))
        case "countries"::ParseKey(country)::tl   => select(params, tl, videos.filter(_.matchesCountry(country)))
        case "videos"::videoId::tl                => select(params, tl, videos.filter(_.videoId == videoId))
        case other                                => (videos, other) 
      }
    }
  }
}
