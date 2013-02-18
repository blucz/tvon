package tvon.server

import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.native.Serialization

import org.iq80.leveldb._
import org.iq80.leveldb.impl.Iq80DBFactory._

import java.io._
import java.nio._
import java.nio.file._

trait Database extends AutoCloseable { }

trait LevelDbDatabaseComponent extends IMDBDatabaseComponent 
                                  with ProfileDatabaseComponent 
                                  with CollectionDatabaseComponent
                                  with BrowseKeyDatabaseComponent 
                                  with Lifecycle { 
  this: ConfigComponent =>
  val db = new LevelDbDatabase(Paths.get(config.datapath).resolve("tvon.db").toString())
  override def shutdown() {
    super.shutdown()
    Log.info("[db] closing")
    db.close()
  }
}

class LevelDbDatabase(datapath: String) extends Database with ProfileDatabase 
                                                         with IMDBDatabase 
                                                         with BrowseKeyDatabase 
                                                         with CollectionDatabase {
  val options  = new Options;
  options.createIfMissing(true);

  val KEYSPACE_VIDEOFILE        : Byte = 1
  val KEYSPACE_IMDBMETADATA     : Byte = 2
  val KEYSPACE_PROFILE          : Byte = 3
  val KEYSPACE_KEY_TO_STRING    : Byte = 4
  val KEYSPACE_STRING_TO_KEY    : Byte = 5

  val leveldb = factory.open(new File(datapath), options);

  implicit val formats = Serialization.formats(NoTypeHints)

  private def mkKey(keyspace: Byte, key: String): Array[Byte] = {
    val strbytes = bytes(key)
    val bb       = ByteBuffer.allocate(strbytes.length + 1)
    bb.put(keyspace)
    bb.put(strbytes)
    bb.array
  }

  private def putString(keyspace: Byte, key: String, value: String) = {
    leveldb.put(mkKey(keyspace, key), bytes(value))
  }

  private def getString(keyspace: Byte, key: String): Option[String] = {
    leveldb.get(mkKey(keyspace, key)) match {
      case null => None
      case bs   => Some(asString(bs))
    }
  }

  private def put[A <: AnyRef](keyspace: Byte, key: String, value: A) = {
    leveldb.put(mkKey(keyspace, key), bytes(Serialization.write(value)))
  }

  private def delete(keyspace: Byte, key: String) {
    leveldb.delete(mkKey(keyspace, key))
  }

  private def get[A <: AnyRef] (keyspace: Byte, key: String) (implicit m:Manifest[A]) : Option[A] = { 
    leveldb.get(mkKey(keyspace, key)) match {
      case null => None
      case bs   => Some(parse(asString(bs)).extract[A])
    }
  }

  private def getAll[A](keyspace: Byte) (implicit m:Manifest[A]): List[A] = {
    val iterator = leveldb.iterator
    iterator.seek(Array[Byte](keyspace))
    var ret = List[A]()
    while (iterator.hasNext && iterator.peekNext.getKey()(0) == keyspace) {
      ret = parse(asString(iterator.peekNext.getValue)).extract[A] :: ret
      iterator.next
    }
    ret
  }

  def putIMDBMetadata(metadata: DatabaseIMDBMetadata) {
    put(KEYSPACE_IMDBMETADATA, metadata.imdb_id, metadata)
  }
  def tryGetIMDBMetadata(imdbId: String): Option[DatabaseIMDBMetadata] = {
    get[DatabaseIMDBMetadata](KEYSPACE_IMDBMETADATA, imdbId)
  }
  def loadIMDBMetadata(): List[DatabaseIMDBMetadata] = {
    getAll[DatabaseIMDBMetadata](KEYSPACE_IMDBMETADATA)
  }
  def deleteIMDBMetadata(imdbId: String) {
    delete(KEYSPACE_IMDBMETADATA, imdbId)
  }

  def tryGetVideo(videoId: String): Option[DatabaseVideo] = {
    get[DatabaseVideo](KEYSPACE_VIDEOFILE, videoId)
  }
  def putVideo(videoFile: DatabaseVideo) {
    put(KEYSPACE_VIDEOFILE, videoFile.videoId, videoFile)
  }
  def loadVideos(): List[DatabaseVideo] = {
    getAll[DatabaseVideo](KEYSPACE_VIDEOFILE)
  }
  def deleteVideo(videoId: String) {
    delete(KEYSPACE_VIDEOFILE, videoId)
  }

  def tryGetProfile(profileId: String): Option[DatabaseProfile] = {
    get[DatabaseProfile](KEYSPACE_PROFILE, profileId)
  }
  def putProfile(profile: DatabaseProfile) {
    put(KEYSPACE_PROFILE, profile.profileId, profile)
  }
  def loadProfiles(): List[DatabaseProfile] = {
    getAll[DatabaseProfile](KEYSPACE_PROFILE)
  }
  def deleteProfile(profileId: String) {
    delete(KEYSPACE_PROFILE, profileId)
  }

  // 
  // map strings into url-safe keys + back
  //
  def keyToString(k:String): Option[String] = {
    getString(KEYSPACE_KEY_TO_STRING, k) 
  }

  def stringToKey(s:String): String = {
    getString(KEYSPACE_STRING_TO_KEY, s) match {
      case Some(k) => k
      case None    => 
        var existing: Option[String] = None
        var ordinal = 0
        var cleankey = Utils.generateKey(s)
        var key      = cleankey 
        do {
          if (ordinal != 0) { key = cleankey + "_" + ordinal }
          existing = getString(KEYSPACE_KEY_TO_STRING, key)
          ordinal += 1
        } while (!existing.isEmpty)
        putString(KEYSPACE_KEY_TO_STRING, key, s)
        putString(KEYSPACE_STRING_TO_KEY, s, key)
        key
    }
  }

  def close() {
    leveldb.close
  }

}
