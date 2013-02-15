package tvon.server

import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.native.Serialization

import org.iq80.leveldb._
import org.iq80.leveldb.impl.Iq80DBFactory._

import java.io._
import java.nio._

class Database(datapath: String) extends AutoCloseable {
  val options = new Options;
  options.createIfMissing(true);

  val KEYSPACE_VIDEOFILE : Byte = 1
  val KEYSPACE_IMDB      : Byte = 2

  val db = factory.open(new File(datapath), options);

  implicit val formats = Serialization.formats(NoTypeHints)

  private def mkKey(keyspace: Byte, key: String): Array[Byte] = {
    val strbytes = bytes(key)
    val bb       = ByteBuffer.allocate(strbytes.length + 1)
    bb.put(keyspace)
    bb.put(strbytes)
    bb.array
  }

  private def put[A <: AnyRef](keyspace: Byte, key: String, value: A) = {
    db.put(mkKey(keyspace, key), bytes(Serialization.write(value)))
  }

  private def get[A <: AnyRef] (keyspace: Byte, key: String) (implicit m:Manifest[A]) : Option[A] = { 
    db.get(mkKey(keyspace, key)) match {
      case null => None
      case bs   => Some(parse(asString(bs)).extract[A])
    }
  }

  def putImdbMetadata(metadata: IMDBMetadataJSON) {
    put(KEYSPACE_IMDB, metadata.imdb_id, metadata)
  }
  def tryGetImdbMetadata(imdbId: String): Option[IMDBMetadataJSON] = {
    get[IMDBMetadataJSON](KEYSPACE_IMDB, imdbId)
  }
  def tryGetVideoFile(videoId: String): Option[VideoFileJSON] = {
    get[VideoFileJSON](KEYSPACE_VIDEOFILE, videoId)
  }
  def putVideoFile(videoFile: VideoFileJSON) {
    put(KEYSPACE_VIDEOFILE, videoFile.videoId, videoFile)
  }

  def loadVideoFiles(): List[VideoFileJSON] = {
    val iterator = db.iterator
    iterator.seek(Array[Byte](KEYSPACE_VIDEOFILE))
    var ret = List[VideoFileJSON]()
    while (iterator.hasNext && iterator.peekNext.getKey()(0) == KEYSPACE_VIDEOFILE) {
      ret = parse(asString(iterator.peekNext.getValue)).extract[VideoFileJSON] :: ret
      iterator.next
    }
    ret
  }

  def close() {
    db.close
  }
}

