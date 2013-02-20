package tvon.server

import java.io._
import java.nio._
import java.nio.file._

trait Database extends AutoCloseable { }

trait LevelDbDatabaseComponent extends MetadataDatabaseComponent 
                                  with ProfileDatabaseComponent 
                                  with CollectionDatabaseComponent
                                  with BrowseKeyDatabaseComponent 
                                  with ImageCacheDatabaseComponent
                                  with Lifecycle { 
  this: ConfigComponent =>
  val db      = new LevelDbDatabase(Paths.get(config.datapath))
  override def shutdown() {
    super.shutdown()
    Log.info("[db] closing")
    db.close()
  }
}

class LevelDbDatabase(datapath: Path) extends Database with ProfileDatabase 
                                                         with MetadataDatabase 
                                                         with BrowseKeyDatabase 
                                                         with ImageCacheDatabase
                                                         with CollectionDatabase {
  import org.iq80.leveldb._
  import org.iq80.leveldb.impl.Iq80DBFactory._
  import LevelDbExtensions._

  val options  = new Options;
  options.createIfMissing(true);

  val KEYSPACE_VIDEOFILE        : Byte = 1
  val KEYSPACE_IMDBMETADATA     : Byte = 2
  val KEYSPACE_PROFILE          : Byte = 3
  val KEYSPACE_KEY_TO_STRING    : Byte = 4
  val KEYSPACE_STRING_TO_KEY    : Byte = 5
  val KEYSPACE_IMDBCACHEDLOOKUP : Byte = 6
  val KEYSPACE_CACHEDIMAGE      : Byte = 7

  val metadatadb: DB = factory.open(datapath.resolve("tvon.db").toFile, options);
  val imagesdb  : DB = factory.open(datapath.resolve("images.db").toFile, options);

  def putIMDBMetadata(metadata: IMDBMetadata) {
    metadatadb.putJson(KEYSPACE_IMDBMETADATA, metadata.imdb_id, metadata)
  }
  def tryGetIMDBMetadata(imdbId: String): Option[IMDBMetadata] = {
    metadatadb.getJson[IMDBMetadata](KEYSPACE_IMDBMETADATA, imdbId)
  }
  def loadIMDBMetadata(): List[IMDBMetadata] = {
    metadatadb.getAll[IMDBMetadata](KEYSPACE_IMDBMETADATA)
  }
  def deleteIMDBMetadata(imdbId: String) {
    metadatadb.delete(KEYSPACE_IMDBMETADATA, imdbId)
  }

  def loadCachedIMDBLookup(key: String): Option[Option[String]] = {
    metadatadb.getString(KEYSPACE_IMDBCACHEDLOOKUP, key) match {
      case None     => None
      case Some("") => Some(None)
      case Some(s)  => Some(Some(s))
    }
  }
  def cacheIMDBLookup(key: String, value: Option[String]) {
    metadatadb.putString(KEYSPACE_IMDBCACHEDLOOKUP, key, value getOrElse "")
  }

  def tryGetVideo(videoId: String): Option[DatabaseVideo] = {
    metadatadb.getJson[DatabaseVideo](KEYSPACE_VIDEOFILE, videoId)
  }
  def putVideo(videoFile: DatabaseVideo) {
    metadatadb.putJson(KEYSPACE_VIDEOFILE, videoFile.videoId, videoFile)
  }
  def loadVideos(): List[DatabaseVideo] = {
    metadatadb.getAll[DatabaseVideo](KEYSPACE_VIDEOFILE)
  }
  def deleteVideo(videoId: String) {
    metadatadb.delete(KEYSPACE_VIDEOFILE, videoId)
  }

  def tryGetProfile(profileId: String): Option[DatabaseProfile] = {
    metadatadb.getJson[DatabaseProfile](KEYSPACE_PROFILE, profileId)
  }
  def putProfile(profile: DatabaseProfile) {
    metadatadb.putJson(KEYSPACE_PROFILE, profile.profileId, profile)
  }
  def loadProfiles(): List[DatabaseProfile] = {
    metadatadb.getAll[DatabaseProfile](KEYSPACE_PROFILE)
  }
  def deleteProfile(profileId: String) {
    metadatadb.delete(KEYSPACE_PROFILE, profileId)
  }

  // 
  // map strings into url-safe keys + back
  //
  def keyToString(k:String): Option[String] = {
    metadatadb.getString(KEYSPACE_KEY_TO_STRING, k) 
  }

  def stringToKey(s:String): String = {
    metadatadb.getString(KEYSPACE_STRING_TO_KEY, s) match {
      case Some(k) => k
      case None    => 
        var existing: Option[String] = None
        var ordinal = 0
        var cleankey = Utils.generateUrlSafeKey(s)
        var key      = cleankey 
        do {
          if (ordinal != 0) { key = cleankey + "_" + ordinal }
          existing = metadatadb.getString(KEYSPACE_KEY_TO_STRING, key)
          ordinal += 1
        } while (!existing.isEmpty)
        metadatadb.putString(KEYSPACE_KEY_TO_STRING, key, s)
        metadatadb.putString(KEYSPACE_STRING_TO_KEY, s, key)
        key
    }
  }

  // 
  // cached image
  //
  object CachedImageBinaryFormat {
    val VERSION = 1
    def fromByteArray(bytes: Array[Byte]): CachedImage = {
      val bytestream = new ByteArrayInputStream(bytes)
      val stream     = new DataInputStream(bytestream)
      val version = stream.readByte()
      if (version != VERSION) throw new Exception("invalid cached image version")
      val mimeType = stream.readUTF()
      val len = stream.readInt()
      val data = new Array[Byte](len)
      stream.read(data, 0, len)
      new CachedImage(mimeType,data)
    }

    def toByteArray(image: CachedImage) : Array[Byte] = {
      var bytestream = new ByteArrayOutputStream
      var stream = new DataOutputStream(bytestream)
      stream.writeByte(VERSION)
      stream.writeUTF(image.mimeType)
      stream.writeInt(image.data.length)
      stream.write(image.data, 0, image.data.length)
      stream.flush()
      bytestream.toByteArray()
    }
  }

  def loadCachedImage(key: String): Option[CachedImage] = {
    imagesdb.getByteArray(KEYSPACE_CACHEDIMAGE, key).map(CachedImageBinaryFormat.fromByteArray(_))
  }

  def cacheImage(key: String, value: CachedImage) {
    imagesdb.putByteArray(KEYSPACE_CACHEDIMAGE, key, CachedImageBinaryFormat.toByteArray(value))
  }

  def close() {
    metadatadb.close()
    imagesdb.close()
  }
}

