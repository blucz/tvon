package tvon.server

import java.net.URLEncoder
import Extensions._
import scala.collection.mutable._

case class QueryParameters(
  screenId:  Option[String],
  profileId: Option[String]
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

    def select(params: QueryParameters, path: List[String], videos: List[Video]): (List[Video],List[String]) = {
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
        case "queue"::tl                          => 
          profiles.get(params.profileId getOrElse "") match {
            case None          => (List(),tl)
            case Some(profile) =>
              val ret = new HashSet[Video]

              // Process the explicit queue
              for (item <- profile.explicitQueue) {
                collection.getVideo(item.videoId) match {
                  case None        => 
                  case Some(video) => 
                    if (!video.watchedSince(profile, item.dateAdded)) {
                      ret.add(video)
                    }
                }
              }

              // Process the auto-add queue
              for (item <- profile.autoQueue) {
                for (video <- select(params, item.path)._1) {
                  if (video.dateAdded.after(item.dateAdded) && !video.watchedSince(profile, item.dateAdded)) {
                    ret.add(video)
                  }
                }
              }

              // return results
              (ret.toList, tl)
          }
        case other                                => (videos, other) 
      }
    }
  }
}
