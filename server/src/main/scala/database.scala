package tvon.server

import org.json4s._
import org.json4s.native.JsonMethods._

class Database(datapath: String) {
  def putImdbMetadata(metadata: IMDBMetadataJSON) {
    throw new UnsupportedOperationException
  }
  def tryGetImdbMetadata(imdbId: String): Option[IMDBMetadataJSON] = {
    throw new UnsupportedOperationException
  }
  def putVideoFile(videoFile: VideoFileJSON) {
    throw new UnsupportedOperationException
  }
  def loadVideoFiles(): List[VideoFileJSON] = {
    throw new UnsupportedOperationException
  }
}

