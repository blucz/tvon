package tvon.server

import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.native.Serialization
import java.net.URLEncoder
import java.util.Date
import scala.collection.mutable._

import scalaj.http.Http
import scalaj.http.HttpOptions

import Extensions._

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
  val date             : Option[Date]) {

  def fixUp: IMDBEpisode = {
    IMDBEpisode(
      title   = title.map(Utils.htmlDecode(_)),
      season  = season,
      episode = episode,
      date    = date
    )
  }
}

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
  val country          : List[String]) {

  def fixUp: IMDBMetadata = {
    IMDBMetadata(
      imdb_id       = imdb_id,
      title          = Utils.htmlDecode(title),
      writers        = writers.map(Utils.htmlDecode(_)),
      actors         = actors.map(Utils.htmlDecode(_)),
      genres         = genres.map(Utils.htmlDecode(_)),
      directors      = directors.map(Utils.htmlDecode(_)),
      `type`         = `type`,
      year           = year,
      episodes       = episodes.map(es => es.map(e => e.fixUp)),
      runtime        = runtime,
      language       = genres.map(Utils.htmlDecode(_)),
      film_locations = film_locations.map(Utils.htmlDecode(_)),
      imdbUrl        = imdbUrl,
      plot           = plot.map(Utils.htmlDecode(_)),
      plot_simple    = plot.map(Utils.htmlDecode(_)),
      poster         = poster,
      rating         = rating,
      rated          = rated,
      rating_count   = rating_count,
      release_date   = release_date,
      country        = country.map(Utils.htmlDecode(_))
    )
  }
}

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
          Thread.sleep(1200)   // sleep so we don't pound the service
          if (WorkQueue.isshutdown) return
          var basereq = Http("http://imdbapi.org/").params("title"   -> video.title,
                                                           "type"    -> "json",
                                                           "plot"    -> "full",
                                                           "limit"   -> "5")
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
              val imdb = results.extract[List[IMDBMetadata]].map(_.fixUp)

              var metadata = imdb.sortBy(r => {
                var points = 0
                if (r.year == video.year) points += 1
                if (Utils.getMergeKey(r.title) == Utils.getMergeKey(video.title)) points += 1
                if (video.isTv && r.`type` == "TVS") points += 1
                if (!video.isTv && r.`type` == "M") points += 1
                -points
              }).head

              Log.trace(s"[metadata] got metadata: ${video.title} (${video.year getOrElse "?"}) ==> ${metadata.title} ${metadata.year.map("("+_.toString+")") getOrElse ""} (${metadata.`type`})")
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

      val inmem_cache: HashMap[String, IMDBMetadata] = new HashMap[String,IMDBMetadata]()

      def loadIMDBMetadata(video: Video): Option[IMDBMetadata] = {
        if (video.title.trim == "") {
          Log.warning("[metadata] ignoring empty title")
          return None
        }
        val key = "v5:" + video.isTv + ":" + (video.year.map(_.toString) getOrElse "?") + ":" + video.title;
        db.loadCachedIMDBLookup(key) match {
          case None               => WorkQueue ! Lookup(video, key); None    // kick off async lookup
          case Some(None)         => None
          case Some(Some(imdbid)) => 
            inmem_cache.get(imdbid) match {
              case None        =>
                db.tryGetIMDBMetadata(imdbid) match {
                  case Some(imdb) => inmem_cache(imdbid) = imdb; Some(imdb)
                  case None       => WorkQueue ! Lookup(video, key); None
                }
              case Some(imdb) => Some(imdb)
            }
        }
      }
  }
}

