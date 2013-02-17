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

  val KEYSPACE_VIDEOFILE        : Byte = 1
  val KEYSPACE_IMDBMETADATA     : Byte = 2
  val KEYSPACE_PROFILE          : Byte = 3

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

  private def delete(keyspace: Byte, key: String) {
    db.delete(mkKey(keyspace, key))
  }

  private def get[A <: AnyRef] (keyspace: Byte, key: String) (implicit m:Manifest[A]) : Option[A] = { 
    db.get(mkKey(keyspace, key)) match {
      case null => None
      case bs   => Some(parse(asString(bs)).extract[A])
    }
  }

  private def getAll[A](keyspace: Byte) (implicit m:Manifest[A]): List[A] = {
    val iterator = db.iterator
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

  def tryGetVideoFile(videoId: String): Option[DatabaseVideoFile] = {
    get[DatabaseVideoFile](KEYSPACE_VIDEOFILE, videoId)
  }
  def putVideoFile(videoFile: DatabaseVideoFile) {
    put(KEYSPACE_VIDEOFILE, videoFile.videoId, videoFile)
  }
  def loadVideoFiles(): List[DatabaseVideoFile] = {
    getAll[DatabaseVideoFile](KEYSPACE_VIDEOFILE)
  }
  def deleteVideoFile(videoId: String) {
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

  def close() {
    db.close
  }
}

