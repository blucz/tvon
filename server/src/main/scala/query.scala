package tvon.server

import java.net.URLEncoder
import Extensions._
import scala.collection.mutable._

case class QueryParameters(
  screenId:  Option[String] = None,
  profileId: Option[String] = None
)

//
// browse paths
//
//     /                      = root
//     /queue/movies/...      = root
//     /movies/decades        = list of decade levels
//     /movies/decades/1990s  =

trait QueryComponent { 
  this: CollectionComponent with ProfilesComponent with BrowseKeyDatabaseComponent =>
  val query = new Query

  class Query {
    // select a list of videos that fall within a path
    def select(params: QueryParameters, path: String): (List[Video],List[String]) = {
      select(params, path.split('/').filter(_ != "").toList)
    }

    def select(params: QueryParameters, path: List[String]): (List[Video],List[String]) = {
      select(params, path, collection.allVideos.toList)
    }

    def matches(params: QueryParameters, path: String, video: Video): Boolean = {
      matches(params, path.split('/').filter(_ != "").toList, video)
    }

    def matches(params: QueryParameters, path: List[String], video: Video): Boolean = {
      path match {
        case Nil                                  => true
        case "movies"::tl                         => if (video.isMovie)                   matches(params, tl, video) else false
        case "tv"::tl                             => if (video.isTv)                      matches(params, tl, video) else false
        case "other"::tl                          => if (video.isOther)                   matches(params, tl, video) else false
        case "decades"::ParseInt(decade)::tl      => if (video.matchesDecade(decade))     matches(params, tl, video) else false
        case "seasons"::ParseInt(season)::tl      => if (video.matchesSeason(season))     matches(params, tl, video) else false
        case "shows"::ParseKey(show)::tl          => if (video.matchesShow(show))         matches(params, tl, video) else false
        case "directors"::ParseKey(director)::tl  => if (video.matchesDirector(director)) matches(params, tl, video) else false
        case "actors"::ParseKey(actor)::tl        => if (video.matchesActor(actor))       matches(params, tl, video) else false
        case "genres"::ParseKey(genre)::tl        => if (video.matchesGenre(genre))       matches(params, tl, video) else false
        case "countries"::ParseKey(country)::tl   => if (video.matchesCountry(country))   matches(params, tl, video) else false
        case "videos"::videoId::tl                => if (video.videoId == videoId)        matches(params, tl, video) else false
        case other                                => false
      }
    }

    def select(params: QueryParameters, path: List[String], videos: List[Video]): (List[Video],List[String]) = {
      path match {
        case Nil                                  => (videos,List[String]())
        case "recent"::ParseInt(n)::tl            => select(params, tl, videos.sortBy(_.dateAdded).reverse.take(n))
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

    object ParseInt {
      def unapply(s:String) : Option[Int] = {
        try   { Some(s.toInt)                        }   
        catch { case e:NumberFormatException => None }
        }
      }

      object ParseKey {
        def unapply(s:String) : Option[String] = db.keyToString(s)
    }
  }
}
