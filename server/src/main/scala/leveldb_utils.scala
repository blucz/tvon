package tvon.server

import org.iq80.leveldb._
import org.iq80.leveldb.impl.Iq80DBFactory._
import java.nio.ByteBuffer

import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.native.Serialization

object LevelDbExtensions {
  import org.iq80.leveldb._
  import org.iq80.leveldb.impl.Iq80DBFactory._

  implicit class RichLevelDbDatabase(db: DB) {
    implicit val formats = Serialization.formats(NoTypeHints)

    private def mkKey(keyspace: Byte, key: String): Array[Byte] = {
      val strbytes = bytes(key)
      val bb       = ByteBuffer.allocate(strbytes.length + 1)
      bb.put(keyspace)
      bb.put(strbytes)
      bb.array
    }

    def putString(keyspace: Byte, key: String, value: String) = {
      db.put(mkKey(keyspace, key), bytes(value))
    }

    def putByteArray(keyspace: Byte, key: String, value: Array[Byte]) = {
      db.put(mkKey(keyspace, key), value)
    }

    def getByteArray(keyspace: Byte, key: String): Option[Array[Byte]] = {
      db.get(mkKey(keyspace, key)) match {
        case null => None
        case bs   => Some(bs)
      }
    }

    def getString(keyspace: Byte, key: String): Option[String] = {
      db.get(mkKey(keyspace, key)) match {
        case null => None
        case bs   => Some(asString(bs))
      }
    }

    def putJson[A <: AnyRef](keyspace: Byte, key: String, value: A) = {
      db.put(mkKey(keyspace, key), bytes(Serialization.write(value)))
    }

    def delete(keyspace: Byte, key: String) {
      db.delete(mkKey(keyspace, key))
    }

    def getJson[A <: AnyRef] (keyspace: Byte, key: String) (implicit m:Manifest[A]) : Option[A] = { 
      db.get(mkKey(keyspace, key)) match {
        case null => None
        case bs   => Some(parse(asString(bs)).extract[A])
      }
    }

    def getAll[A](keyspace: Byte) (implicit m:Manifest[A]): List[A] = {
      val iterator = db.iterator
      iterator.seek(Array[Byte](keyspace))
      var ret = List[A]()
      while (iterator.hasNext && iterator.peekNext.getKey()(0) == keyspace) {
        ret = parse(asString(iterator.peekNext.getValue)).extract[A] :: ret
        iterator.next
      }
      ret
    }
  }
}
