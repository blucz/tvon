package tvon.server

import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.native.Serialization
import java.net.URLEncoder
import java.util.Date

import scalaj.http.Http
import scalaj.http.HttpOptions

trait MetadataDatabaseComponent       { val db: MetadataDatabase       }
trait MetadataDatabase extends Database {
    def putIMDBMetadata(metadata: IMDBMetadata)
    def tryGetIMDBMetadata(imdbId: String): Option[IMDBMetadata]
    def loadIMDBMetadata(): List[IMDBMetadata]
    def deleteIMDBMetadata(imdbId: String)

    def loadCachedIMDBLookup(key: String): Option[Option[String]]
    def cacheIMDBLookup(key: String, value: Option[String])
}

case class IMDBEpisode(
  val title            : Option[String],
  val season           : Int,
  val episode          : Int,
  val date             : Option[Date]   
)

case class IMDBMetadata(
  val imdb_id          : String,
  val title            : String,
  val writers          : List[String],
  val actors           : List[String],
  val genres           : List[String],
  val directors        : List[String],
  val `type`           : String,
  val year             : Option[Int],
  val episodes         : Option[List[IMDBEpisode]],
  val runtime          : List[String],
  val language         : List[String],
  val film_locations   : List[String],
  val imdbUrl          : Option[String],
  val plot             : Option[String],
  val plot_simple      : Option[String],
  val poster           : Option[String],
  val rating           : Option[Double],
  val rated            : Option[String],
  val rating_count     : Option[Int],
  val release_date     : Option[Int],
  val country          : List[String]
)

trait MetadataLookupComponent extends Lifecycle { 
  this: CollectionComponent with MetadataDatabaseComponent => 
  import scala.actors.Actor._

  val metadatalookup = new MetadataLookup

  override def init() {
    Log.info("[metadata] starting")
    metadatalookup.WorkQueue.start()
    super.init()
  }

  override def shutdown() {
    super.shutdown()
    Log.info("[metadata] shutting down")
    metadatalookup.WorkQueue.stop()
  }


  // messages for workqueue
  case class Lookup(video: Video, key: String)
  class MetadataLookup {
      implicit val formats = Serialization.formats(NoTypeHints)

      object WorkQueue extends scala.actors.DaemonActor {
        case object Exit
        var isshutdown = false
        def act() { 
          loop { 
            react { 
              case Exit               => exit()
              case Lookup(video, key) => doLookup(video, key)
            } 
          } 
        }
        def stop() {
          isshutdown = true
          this ! Exit
        }
      }

      def doLookup(video: Video, key: String) {
        try {
          if (WorkQueue.isshutdown) return
          db.loadCachedIMDBLookup(key) match {
            case Some(Some(imdbid)) => 
              db.tryGetIMDBMetadata(imdbid) match {
                case Some(metadata) => 
                  collection.updateIMDBMetadata(video, Some(metadata))
                  return
                case _ => 
              }
            case _ => 
          }
          Thread.sleep(2000)   // sleep so we don't pound the service
          if (WorkQueue.isshutdown) return
          var basereq = if (video.isTv) {
            Http("http://imdbapi.org/").params("title"   -> video.title,
                                               "type"    -> "json",
                                               "plot"    -> "full",
                                               "limit"   -> "3",
                                               "mt"      -> "TVS")
          } else {
            Http("http://imdbapi.org/").params("title"   -> video.title,
                                               "type"    -> "json",
                                               "plot"    -> "full",
                                               "limit"   -> "3",
                                               "mt"      -> "M")
          }
          val json = basereq.headers("User-Agent" -> "tvon",
                                     "Accept"     -> "application/json")
                            .option(HttpOptions.connTimeout(30000))
                            .option(HttpOptions.readTimeout(30000))
                            .asString
          if (WorkQueue.isshutdown) return

          //Log.debug("got: \n" + json)
          parse(json) match {
            case JObject(List(JField("code", JInt(code)), JField("error", JString(error)))) =>
              Log.trace(s"[metadata] got nothing: ${video.title} ==> None")
              db.cacheIMDBLookup(key, None)
              collection.updateIMDBMetadata(video, None)
            case results =>
              val imdb = results.extract[List[IMDBMetadata]]

              val metadata = imdb.head     // XXX: search all results + pick the best
              Log.trace(s"[metadata] got metadata: ${video.title} ==> ${metadata.title} ${metadata.year.map("("+_.toString+")") getOrElse ""}")
              db.putIMDBMetadata(metadata)
              db.cacheIMDBLookup(key, Some(metadata.imdb_id))
              collection.updateIMDBMetadata(video, Some(metadata))
          }

        } catch {
          case e:InterruptedException => return
          case e:Exception =>
            Log.warning(s"[metadata] error accessing imdb for ${video.title}: ${e}")
            e.printStackTrace
        }
      }

      def loadIMDBMetadata(video: Video): Option[IMDBMetadata] = {
        if (video.title.trim == "") {
          Log.warning("[metadata] ignoring empty title")
          return None
        }
        val key = "v1:" + video.isTv + ":" + video.title;
        db.loadCachedIMDBLookup(key) match {
          case None               => WorkQueue ! Lookup(video, key); None    // kick off async lookup
          case Some(None)         => None
          case Some(Some(imdbid)) => db.tryGetIMDBMetadata(imdbid) match {
            case Some(imdb) => Some(imdb)
            case None       => WorkQueue ! Lookup(video, key); None
          }
        }
      }
  }
}

