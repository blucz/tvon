package tvon.server

import scalaj.http.Http
import scalaj.http.HttpOptions

trait ImageCacheDatabaseComponent       { val db: ImageCacheDatabase       }
trait ImageCacheDatabase extends Database {
    def loadCachedImage(key: String): Option[CachedImage]
    def cacheImage(key: String, value: CachedImage)
}

class CachedImage(val mimeType: String, val data: Array[Byte])

trait ImageCacheComponent {
  this: ImageCacheDatabaseComponent =>

  val imagecache = new ImageCache
  class ImageCache {
    private val VERSION_PREFIX = "v1:"

    def getImage(url: String): Option[CachedImage] = {
      try {
        val key = VERSION_PREFIX + url;
        db.loadCachedImage(key) match {
          case Some(image) => Some(image)
          case None        => 
            Http(url).option(HttpOptions.connTimeout(30000))
                     .option(HttpOptions.readTimeout(30000))
                     .headers("User-Agent" -> "tvon")
                     .asHeadersAndParse(Http.readBytes) match {
            case (200,headers,data) => 
              val cachedimage = new CachedImage(headers("Content-Type").head, data)
              db.cacheImage(key, cachedimage)
              Some(cachedimage)
            case (code,_,_) => 
              throw new Exception("HTTP request to " + url + " failed with code " + code);
          }
        }
      } catch {
        case e:Exception => Log.warning(s"[imagecache] error loading image", e);
                            None
      }
    }
  }
}
