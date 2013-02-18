package tvon.server

case class ApiBrowseAction {
}

case class ApiBrowseItem(
  path:     String,
  title:    String,
  subtitle: String                = "",
  actions:  List[ApiBrowseAction] = List[ApiBrowseAction]()
)

case class ApiBrowseLevel(
  item:     ApiBrowseItem,
  subitems: List[ApiBrowseItem]
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

class Browser(val manager: Manager) {
  val collection = manager.collection
  val profiles   = manager.profiles
  val db         = manager.db

  private def browse_root(params: BrowseParameters): Option[ApiBrowseLevel] = {
    val item = ApiBrowseItem(path     = "/",
                             title    = "Browse",
                             subtitle = "")
    val subitems = List[ApiBrowseItem](
      ApiBrowseItem(path = "/queue",            title = s"My Queue"),
      ApiBrowseItem(path = "/movies/filters",   title = s"Movies"),
      ApiBrowseItem(path = "/tv/shows",         title = s"TV"),    
      ApiBrowseItem(path = "/other",            title = s"Other")
    )
    finishBrowse(item, subitems)
  }

  private def browse_filters(params: BrowseParameters, videos: List[VideoFile]): Option[ApiBrowseLevel] = {
    val item = ApiBrowseItem(path     = "/",
                             title    = "Movies",
                             subtitle = "")
    Log.info("FILTERS")
    val subitems = List[ApiBrowseItem](
      ApiBrowseItem(path = "/movies/recentlyadded", title = s"Recently Added"),
      ApiBrowseItem(path = "/movies",               title = s"All"),
      ApiBrowseItem(path = "/movies/decades",       title = s"Decades"),    
      ApiBrowseItem(path = "/movies/actors",        title = s"Actors"),
      ApiBrowseItem(path = "/movies/directors",     title = s"Directors"),
      ApiBrowseItem(path = "/movies/genres",        title = s"Genres"),
      ApiBrowseItem(path = "/movies/countries",     title = s"Countries")
    )
    finishBrowse(item, subitems)
  }

  // generate a browse level within a path
  def browse(params: BrowseParameters): Option[ApiBrowseLevel] = {
    val comps = params.path.split('/').filter(_ != "").toList
    if (comps.length == 0) {
      browse_root(params)
    } else {
      browse(params, comps, collection.videos.values.toList)
    }
  }

  // select a list of videos that fall within a path
  def select(params: BrowseParameters): (List[VideoFile],List[String]) = {
    select(params, params.path.split('/').filter(_ != "").toList)
  }

  private def join(a:String, b:String) = a + "/" + b

  private def videoSubtitle(v: VideoFile): String = {
    if (v.isTv) {
      val e = v.episodes match {
        case Nil         => "?"
        case e1::e2::Nil => s"${e1}-${e2}"
        case es          => es mkString ", "
      }
      val s = v.season getOrElse "?"
      s"Season ${s}, Episode ${e}"
    } else {
      v.year.map(_.toString) getOrElse ""
    }
  }

  private def browse_videos(params: BrowseParameters, videos: List[VideoFile]): Option[ApiBrowseLevel] = {
    if (videos.length == 0) { return None }
    val item     = ApiBrowseItem(path = params.path, title = s"${videos.length} Results")
    val subitems = videos.map(v => ApiBrowseItem(
      path     = join("videos", v.videoId),
      title    = v.title,
      subtitle = videoSubtitle(v)
    ))
    finishBrowse(item, subitems)
  }

  private def browse_decades(params: BrowseParameters, videos: List[VideoFile]): Option[ApiBrowseLevel] = {
    val item     = ApiBrowseItem(path = params.path, title = "Decades")
    val subitems = videos.filter(!_.decade.isEmpty).map(_.decade.get).map(decade =>
        ApiBrowseItem(path = join(params.path, decade.toString), title = s"${decade}s")
    )
    finishBrowse(item, subitems)
  }

  private def browse_genres(params: BrowseParameters, videos: List[VideoFile]): Option[ApiBrowseLevel] = {
    None
  }

  private def browse_actors(params: BrowseParameters, videos: List[VideoFile]): Option[ApiBrowseLevel] = {
    None
  }

  private def browse_directors(params: BrowseParameters, videos: List[VideoFile]): Option[ApiBrowseLevel] = {
    None
  }

  private def browse_countries(params: BrowseParameters, videos: List[VideoFile]): Option[ApiBrowseLevel] = {
    None
  }

  private def browse_seasons(params: BrowseParameters, videos: List[VideoFile]): Option[ApiBrowseLevel] = {
    val item     = ApiBrowseItem(path = params.path, title = "Seasons")
    val subitems = videos.filter(!_.season.isEmpty).map(_.season.get).map(season =>
        ApiBrowseItem(path = join(params.path, season.toString), title = s"Season ${season}")
    )
    finishBrowse(item, subitems)
  }

  private def browse_shows(params: BrowseParameters, videos: List[VideoFile]): Option[ApiBrowseLevel] = {
    val item     = ApiBrowseItem(path = params.path, title = "Shows")
    val subitems = videos.filter(_.isTv).map(_.title).distinct.sorted.map(title => 
        ApiBrowseItem(path = join(params.path, db.stringToKey(title)), title = title)
    )
    finishBrowse(item, subitems)
  }

  private def finishBrowse(item: ApiBrowseItem, subitems: List[ApiBrowseItem]): Option[ApiBrowseLevel] = {
    subitems match {
      case Nil => None
      case _   => Some(ApiBrowseLevel(item, subitems))
    }
  }
  

  private def browse(params: BrowseParameters, fullpath: List[String], invideos: List[VideoFile]): Option[ApiBrowseLevel] = {
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

  private def select(params: BrowseParameters, path: List[String]): (List[VideoFile],List[String]) = {
    select(params, path, collection.videos.values.toList)
  }

  private def select(params: BrowseParameters, path: List[String], videos: List[VideoFile]): (List[VideoFile],List[String]) = {
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
      case other                                => (videos, other) 
    }
  }
}
